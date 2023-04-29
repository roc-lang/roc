{
  "targets": [
    {
      "target_name": "addon",
      "sources": [ "demo.c" ],
      "libraries": [
          "-lhello",
          "-L<(module_root_dir)"
      ]
    }
  ]
}
