app [program] { pf: platform "./platform.roc" }

program = { init: || { seconds: 7.U64 }, count: |m| m.seconds }
