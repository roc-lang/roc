# Fixtures

Each directory holds a `case.py`: one Python expression that builds a `gen.Prog` from the model's own type and value
constructors. `./run.sh check <dir>` writes it out as a test module and as apps, runs every config, and compares with
the model. A `known.txt` names the configs an open issue makes disagree (one per line, `#` starts a comment); those
must disagree and every other config must agree.
