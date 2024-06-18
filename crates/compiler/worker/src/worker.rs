use crossbeam::{
    channel::{Receiver, SendError, Sender},
    deque::{Injector, Stealer, Worker},
};
use roc_collections::MutSet;
use roc_module::symbol::ModuleId;
use roc_work::Phase;
use std::ops::ControlFlow;

#[derive(Debug)]
pub enum WorkerMsg {
    Shutdown,
    TaskAdded,
}

#[derive(Debug)]
pub enum ChannelProblem {
    FailedToSendRootMsg,
    FailedToSendWorkerShutdownMsg,
    ChannelDisconnected,
    FailedToSendManyMsg,
    FailedToSendFinishedSpecializationsMsg,
    FailedToSendTaskMsg,
    FailedToSendFinishedTypeCheckingMsg,
    FailedToEnqueueTask,
}

pub fn worker_task_step<Task>(
    worker: &Worker<Task>,
    injector: &Injector<Task>,
    stealers: &[Stealer<Task>],
    worker_msg_rx: &Receiver<WorkerMsg>,
    run_task: impl Fn(Task) -> Result<(), ChannelProblem>,
) -> Result<ControlFlow<(), ()>, ChannelProblem> {
    match worker_msg_rx.try_recv() {
        Ok(msg) => {
            match msg {
                WorkerMsg::Shutdown => {
                    // We've finished all our work. It's time to
                    // shut down the thread, so when the main thread
                    // blocks on joining with all the worker threads,
                    // it can finally exit too!
                    Ok(ControlFlow::Break(()))
                }
                WorkerMsg::TaskAdded => {
                    // Find a task - either from this thread's queue,
                    // or from the main queue, or from another worker's
                    // queue - and run it.
                    //
                    // There might be no tasks to work on! That could
                    // happen if another thread is working on a task
                    // which will later result in more tasks being
                    // added. In that case, do nothing, and keep waiting
                    // until we receive a Shutdown message.
                    if let Some(task) = find_task(worker, injector, stealers) {
                        run_task(task)?;
                    }

                    Ok(ControlFlow::Continue(()))
                }
            }
        }
        Err(err) => match err {
            crossbeam::channel::TryRecvError::Empty => Ok(ControlFlow::Continue(())),
            crossbeam::channel::TryRecvError::Disconnected => {
                Err(ChannelProblem::ChannelDisconnected)
            }
        },
    }
}

pub fn worker_task<Task>(
    worker: Worker<Task>,
    injector: &Injector<Task>,
    stealers: &[Stealer<Task>],
    worker_msg_rx: crossbeam::channel::Receiver<WorkerMsg>,
    run_task: impl Fn(Task) -> Result<(), ChannelProblem>,
) -> Result<(), ChannelProblem> {
    // Keep listening until we receive a Shutdown msg
    for msg in worker_msg_rx.iter() {
        match msg {
            WorkerMsg::Shutdown => {
                // We've finished all our work. It's time to
                // shut down the thread, so when the main thread
                // blocks on joining with all the worker threads,
                // it can finally exit too!
                return Ok(());
            }
            WorkerMsg::TaskAdded => {
                // Find a task - either from this thread's queue,
                // or from the main queue, or from another worker's
                // queue - and run it.
                //
                // There might be no tasks to work on! That could
                // happen if another thread is working on a task
                // which will later result in more tasks being
                // added. In that case, do nothing, and keep waiting
                // until we receive a Shutdown message.
                if let Some(task) = find_task(&worker, injector, stealers) {
                    run_task(task)?;
                }
            }
        }
    }

    Ok(())
}

pub fn start_tasks<State, Task, Tasks: IntoIterator<Item = Task>>(
    state: &mut State,
    work: MutSet<(ModuleId, Phase)>,
    injector: &Injector<Task>,
    worker_listeners: &[Sender<WorkerMsg>],
    mut start_phase: impl FnMut(ModuleId, Phase, &mut State) -> Tasks,
) -> Result<(), SendError<WorkerMsg>> {
    for (module_id, phase) in work {
        let tasks = start_phase(module_id, phase, state);

        for task in tasks {
            injector.push(task);

            for listener in worker_listeners {
                listener.send(WorkerMsg::TaskAdded)?;
            }
        }
    }

    Ok(())
}

/// Find a task according to the following algorithm:
///
/// 1. Look in a local Worker queue. If it has a task, pop it off the queue and return it.
/// 2. If that queue was empty, ask the global queue for a task.
/// 3. If the global queue is also empty, iterate through each Stealer (each Worker queue has a
///    corresponding Stealer, which can steal from it. Stealers can be shared across threads.)
///
/// Based on https://docs.rs/crossbeam/0.7.3/crossbeam/deque/index.html#examples
pub fn find_task<T>(local: &Worker<T>, global: &Injector<T>, stealers: &[Stealer<T>]) -> Option<T> {
    // Pop a task from the local queue, if not empty.
    local.pop().or_else(|| {
        // Otherwise, we need to look for a task elsewhere.
        core::iter::repeat_with(|| {
            // Try stealing a task from the global queue.
            global
                .steal()
                // Or try stealing a task from one of the other threads.
                .or_else(|| stealers.iter().map(|s| s.steal()).collect())
        })
        // Loop while no task was stolen and any steal operation needs to be retried.
        .find(|s| !s.is_retry())
        // Extract the stolen task, if there is one.
        .and_then(|s| s.success())
    })
}
