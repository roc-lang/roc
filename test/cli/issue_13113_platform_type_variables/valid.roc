app [program] { pf: platform "./valid_platform.roc" }

program = { init: |{}| Box.box({ seconds: 0.F32 }) }
