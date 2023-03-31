{
  "targets": [
    {
      "target_name": "addon",
      "sources": [ "platform/glue/demo.c" ],
      "libraries": [
          "-lhello",
          "-L<(module_root_dir)"
      ]
    }
  ]
}
