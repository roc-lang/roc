/*
 * lib/kprobe-coverage.c
 *
 * Copyright (C) 2014 Simon Kagstrom
 *
 * Author: Simon Kagstrom <simon.kagstrom@gmail.com>
 *
 * This file is subject to the terms and conditions of the GNU General Public
 * License.  See the file COPYING in the main directory of this archive
 * for more details.
 */
#include <linux/module.h>
#include <linux/kprobes.h>
#include <linux/list.h>
#include <linux/mutex.h>
#include <linux/debugfs.h>
#include <linux/workqueue.h>
#include <linux/wait.h>
#include <linux/fs.h>
#include <linux/vmalloc.h>
#include <linux/slab.h>
#include <linux/seq_file.h>
#include <linux/uaccess.h>

struct kprobe_coverage
{
	struct dentry *debugfs_root;

	struct workqueue_struct *workqueue;
	wait_queue_head_t wait_queue;

	/* Each coverage entry is on exactly one of these lists */
	struct list_head deferred_list; /* Probes for not-yet-loaded-modules */

	struct list_head pending_list;  /* Probes which has not yet triggered */
	struct list_head hit_list;      /* Triggered probes awaiting readout */

	const char *module_names[32];
	unsigned int name_count;

	struct mutex lock;
};

struct kprobe_coverage_entry
{
	struct kprobe kp;
	int name_index; /* an index into the name table above (0 is always the kernel */
	unsigned long base_addr;

	struct list_head lh;
	struct work_struct work;
};

static struct kprobe_coverage *global_kpc;

/* Return index into module name vector. Should be called with
 * mutex held. */
static int module_name_to_index(struct kprobe_coverage *kpc,
		const char *module_name)
{
	int i;

	BUG_ON(!mutex_is_locked(&kpc->lock));

	/* The first is always the kernel (NULL) */
	if (module_name == NULL)
		return 0;

	for (i = 1; i < kpc->name_count; i++) {
		if (strcmp(kpc->module_names[i], module_name) == 0)
			return i;
	}

	return -1;
}

static int kpc_allocate_module_name_index(struct kprobe_coverage *kpc,
		const char *module_name)
{
	int index;

	mutex_lock(&kpc->lock);
	index = module_name_to_index(kpc, module_name);

	/* Not found? If so allocate new entry unless vector is full */
	if (index < 0 && kpc->name_count < ARRAY_SIZE(kpc->module_names)) {
		index = kpc->name_count;

		kpc->module_names[index] = kstrdup(module_name, GFP_KERNEL);
		kpc->name_count++;
	}
	mutex_unlock(&kpc->lock);

	return index;

}

static int kpc_pre_handler(struct kprobe *kp, struct pt_regs *regs)
{
	struct kprobe_coverage_entry *entry = (struct kprobe_coverage_entry *)
					container_of(kp, struct kprobe_coverage_entry, kp);

	/* Schedule it for removal */
	queue_work(global_kpc->workqueue, &entry->work);

	return 0;
}

static void kpc_probe_work(struct work_struct *work)
{
	struct kprobe_coverage_entry *entry = (struct kprobe_coverage_entry *)
					container_of(work, struct kprobe_coverage_entry, work);
	struct kprobe_coverage *kpc = global_kpc;

	unregister_kprobe(&entry->kp);

	/* Move from pending list to the hit list */
	mutex_lock(&kpc->lock);

	list_del(&entry->lh);
	list_add_tail(&entry->lh, &kpc->hit_list);

	/* Wake up the listener */
	wake_up(&kpc->wait_queue);

	mutex_unlock(&kpc->lock);
}


static void free_entry(struct kprobe_coverage_entry *entry)
{
	kfree(entry);
}

static struct kprobe_coverage_entry *new_entry(struct kprobe_coverage *kpc,
		const char *module_name, struct module *mod, unsigned long where)
{
	struct kprobe_coverage_entry *out;
	unsigned long base_addr = 0;

	out = kzalloc(sizeof(*out), GFP_KERNEL);
	if (!out)
		return NULL;

	/* Setup the base address if we know it, otherwise keep at 0 */
	if (mod)
		base_addr = (unsigned long)mod->module_core;

	out->name_index = kpc_allocate_module_name_index(kpc, module_name);
	out->base_addr = base_addr;
	out->kp.addr = (void *)(base_addr + where);
	out->kp.pre_handler = kpc_pre_handler;
	INIT_WORK(&out->work, kpc_probe_work);

	return out;
}

/* Called with the lock held */
static int enable_probe(struct kprobe_coverage *kpc,
		struct kprobe_coverage_entry *entry)
{
	int err;

	BUG_ON(!mutex_is_locked(&kpc->lock));

	/* Done before enable_kprobe so that it's really on the list if
	 * triggered */
	list_add(&entry->lh, &kpc->pending_list);

	if ( (err = register_kprobe(&entry->kp)) < 0) {
		list_del(&entry->lh);
		return -EINVAL;
	}

	return 0;
}

/* Called with the lock held */
static void defer_probe(struct kprobe_coverage *kpc,
		struct kprobe_coverage_entry *entry)
{
	list_add(&entry->lh, &kpc->deferred_list);
}

static void kpc_add_probe(struct kprobe_coverage *kpc, const char *module_name,
		unsigned long where)
{
	struct module *module = NULL;
	struct kprobe_coverage_entry *entry;
	int rv = 0;

	/* Lookup the module which should be instrumented */
	if (module_name) {
		preempt_disable();
		module = find_module(module_name);
		preempt_enable();
	}

	entry = new_entry(kpc, module_name, module, where);

	/* Three cases:
	 *
	 * 1. pending module - module_name is !NULL, module is NULL: Defer
	 *    instrumentation
	 *
	 * 2. vmlinux - module_name and module is NULL: Instrument directly
	 *
	 * 3. loaded module - module_name and module is !NULL: Instrument directly
	 */
	mutex_lock(&kpc->lock);
	if (module_name && !module)
		defer_probe(kpc, entry);
	else
		rv = enable_probe(kpc, entry);
	mutex_unlock(&kpc->lock);

	/* Probe enabling might fail, just free the entry */
	if (rv < 0)
		free_entry(entry);
}

/* Called with lock held */
static void clear_list(struct kprobe_coverage *kpc,
		struct list_head *list, int do_unregister)
{
	struct list_head *iter;
	struct list_head *tmp;

	BUG_ON(!mutex_is_locked(&kpc->lock));

	list_for_each_safe(iter, tmp, list) {
		struct kprobe_coverage_entry *entry;

		entry = (struct kprobe_coverage_entry *)container_of(iter,
				struct kprobe_coverage_entry, lh);
		if (do_unregister)
			unregister_kprobe(&entry->kp);
		list_del(&entry->lh);
		free_entry(entry);
	}
}

static void kpc_clear(struct kprobe_coverage *kpc)
{
	int i;

	/* Free everything on the lists */
	mutex_lock(&kpc->lock);

	clear_list(kpc, &kpc->deferred_list, 0);
	clear_list(kpc, &kpc->hit_list, 1);
	clear_list(kpc, &kpc->pending_list, 1);

	INIT_LIST_HEAD(&kpc->deferred_list);
	INIT_LIST_HEAD(&kpc->pending_list);
	INIT_LIST_HEAD(&kpc->hit_list);

	for (i = 0; i < kpc->name_count; i++) {
		kfree(kpc->module_names[i]);

		kpc->module_names[i] = NULL;
	}
	/* Kernel is still at 0 */
	kpc->name_count = 1;

	mutex_unlock(&kpc->lock);
}

static void *kpc_unlink_next(struct kprobe_coverage *kpc)
{
	struct kprobe_coverage_entry *entry = NULL;

	/* Remove the next entry, if any */
	mutex_lock(&kpc->lock);
	if (!list_empty(&kpc->hit_list)) {
		entry = list_first_entry(&kpc->hit_list, struct kprobe_coverage_entry, lh);
		list_del(&entry->lh);
	}
	mutex_unlock(&kpc->lock);

	return entry;
}

static void *kpc_seq_start(struct seq_file *s, loff_t *pos)
{
	struct kprobe_coverage *kpc = s->private;
	int rv;

	 /* Wait for something to arrive on the hit list, abort on signal */
	rv = wait_event_interruptible(kpc->wait_queue, !list_empty(&kpc->hit_list));
	if (rv< 0)
		return NULL;

	return kpc_unlink_next(kpc);
}

static void *kpc_seq_next(struct seq_file *s, void *v, loff_t *pos)
{
	struct kprobe_coverage *kpc = s->private;

	/* Returns NULL if there is nothing more to handle */
	return kpc_unlink_next(kpc);
}

static void kpc_seq_stop(struct seq_file *s, void *v)
{
}

static int kpc_seq_show(struct seq_file *s, void *v)
{
	struct kprobe_coverage *kpc = s->private;
	struct kprobe_coverage_entry *entry = v;
	const char *module_name;
	unsigned long addr;

	/* Lookup the module name from the module table */
	module_name = kpc->module_names[entry->name_index];
	addr = (unsigned long)entry->kp.addr - entry->base_addr;

	seq_printf(s, "%s%s0x%016lx\n",
			module_name ? module_name : "",
			module_name ? ":" : "",
			addr);

	free_entry(entry);

	return 0;
}

static struct seq_operations kpc_seq_ops =
{
	.start = kpc_seq_start,
	.next  = kpc_seq_next,
	.stop  = kpc_seq_stop,
	.show  = kpc_seq_show
};

static int kpc_show_open(struct inode *inode, struct file *file)
{
	struct kprobe_coverage *kpc = inode->i_private;
	int rv;

	rv = seq_open(file, &kpc_seq_ops);

	if (rv == 0) {
		struct seq_file *s = file->private_data;

		s->private = kpc;
	}

	return rv;
}


static ssize_t kpc_control_write(struct file *file, const char __user *user_buf,
		size_t count, loff_t *off)
{
	struct kprobe_coverage *kpc = file->private_data;
	ssize_t out = 0;
	char *line;
	char *buf;
	char *p;

	if (count > PAGE_SIZE)
		return -E2BIG;

	/* Assure it's NULL terminated */
	buf = (char *)kzalloc(count + 1, GFP_KERNEL);
	if (!buf)
		return -ENOMEM;

	if (copy_from_user(buf, user_buf, count)) {
		kfree(buf);
		return -EFAULT;
	}
	p = buf;

	while ( (line = strsep(&p, "\r\n")) ) {
		unsigned long addr;
		char *module = NULL; /* Assume for the kernel */
		char *colon;
		char *addr_p;
		char *endp;

		if (!*line || !p)
			break;

		out += p - line;

		if (strcmp(line, "clear") == 0) {
			kpc_clear(kpc);
			break;
		}
		colon = strstr(line, ":");
		if (colon) {
			/* For a module */
			addr_p = colon + 1;
			*colon = '\0';
			module = line;
		} else
			addr_p = line;

		addr = simple_strtoul(addr_p, &endp, 16);
		if (endp == addr_p || *endp != '\0')
			continue;

		kpc_add_probe(kpc, module, addr);
	}

	kfree(buf);
	*off += out;

	return out;
}

static int kpc_control_open(struct inode *inode, struct file *file)
{
	file->private_data = inode->i_private; /* kpc */

	return 0;
}

static void kpc_handle_coming_module(struct kprobe_coverage *kpc,
		struct module *mod)
{
	struct list_head *iter;
	struct list_head *tmp;

	mutex_lock(&kpc->lock);
	list_for_each_safe(iter, tmp, &kpc->deferred_list) {
		struct kprobe_coverage_entry *entry;

		entry = (struct kprobe_coverage_entry *)container_of(iter,
				struct kprobe_coverage_entry, lh);

		if (module_name_to_index(kpc, mod->name) != entry->name_index)
			continue;

		/* Move the deferred entry to the pending list and enable */
		list_del(&entry->lh);
		entry->base_addr = (unsigned long)mod->module_core;
		entry->kp.addr += entry->base_addr;

		if (enable_probe(kpc, entry) < 0)
			free_entry(entry);
	}
	mutex_unlock(&kpc->lock);
}

static void kpc_handle_going_module(struct kprobe_coverage *kpc,
		struct module *mod)
{
	struct list_head *iter;
	struct list_head *tmp;

	mutex_lock(&kpc->lock);
	list_for_each_safe(iter, tmp, &kpc->pending_list) {
		struct kprobe_coverage_entry *entry;

		entry = (struct kprobe_coverage_entry *)container_of(iter,
				struct kprobe_coverage_entry, lh);

		if (module_name_to_index(kpc, mod->name) != entry->name_index)
			continue;

		/* Remove pending entries for the current module */
		unregister_kprobe(&entry->kp);
		list_del(&entry->lh);
		free_entry(entry);
	}
	mutex_unlock(&kpc->lock);
}

static int kpc_module_notifier(struct notifier_block *nb,
		unsigned long event, void *data)
{
	struct module *mod = data;

	if (event == MODULE_STATE_COMING)
		kpc_handle_coming_module(global_kpc, mod);
	else if (event == MODULE_STATE_GOING)
		kpc_handle_going_module(global_kpc, mod);

	return 0;
}


static const struct file_operations kpc_control_fops =
{
	.owner = THIS_MODULE,
	.open = kpc_control_open,
	.write = kpc_control_write
};

static const struct file_operations kpc_show_fops =
{
	.owner = THIS_MODULE,
	.open = kpc_show_open,
	.read = seq_read,
	.llseek = seq_lseek,
	.release = seq_release,
};



static struct notifier_block kpc_module_notifier_block =
{
	.notifier_call = kpc_module_notifier,
};

static int __init kpc_init(struct kprobe_coverage *kpc)
{
	kpc->workqueue = create_singlethread_workqueue("kprobe-coverage");

	if (!kpc->workqueue)
		return -ENODEV;

	/* Create debugfs entries */
	kpc->debugfs_root = debugfs_create_dir("kprobe-coverage", NULL);
	if (!kpc->debugfs_root) {
		printk(KERN_ERR "kprobe-coverage: creating root dir failed\n");
		goto out_workqueue;
	}
	if (!debugfs_create_file("control", 0200, kpc->debugfs_root, kpc,
			&kpc_control_fops))
		goto out_files;

	if (!debugfs_create_file("show", 0400, kpc->debugfs_root, kpc,
			&kpc_show_fops))
		goto out_files;

	if (register_module_notifier(&kpc_module_notifier_block) < 0)
		goto out_files;

	INIT_LIST_HEAD(&kpc->pending_list);
	INIT_LIST_HEAD(&kpc->hit_list);
	INIT_LIST_HEAD(&kpc->deferred_list);

	init_waitqueue_head(&kpc->wait_queue);
	mutex_init(&kpc->lock);

	/* The kernel is always index 0 */
	kpc->name_count = 1;

	return 0;

out_files:
	debugfs_remove_recursive(kpc->debugfs_root);
out_workqueue:
	destroy_workqueue(kpc->workqueue);

	return -EINVAL;
}

static int __init kpc_init_module(void)
{
	int out;

	global_kpc = kzalloc(sizeof(*global_kpc), GFP_KERNEL);
	if (!global_kpc)
		return -ENOMEM;

	out = kpc_init(global_kpc);
	if (out < 0) {
		kfree(global_kpc);

		global_kpc = NULL;
	}

	return out;
}

static void __exit kpc_exit_module(void)
{
	kpc_clear(global_kpc);

	debugfs_remove_recursive(global_kpc->debugfs_root);
	unregister_module_notifier(&kpc_module_notifier_block);
	destroy_workqueue(global_kpc->workqueue);

	kfree(global_kpc);
	global_kpc = NULL;
}

module_init(kpc_init_module);
module_exit(kpc_exit_module);
MODULE_AUTHOR("Simon Kagstrom <simon.kagstrom@gmail.com>");
MODULE_DESCRIPTION("Code coverage through kprobes and debugfs");
MODULE_LICENSE("GPL");
