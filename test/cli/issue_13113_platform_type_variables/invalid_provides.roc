app [program] { pf: platform "./invalid_provides_platform.roc" }

program = { init: |{}| Box.box({ seconds: 0.F32 }) }
