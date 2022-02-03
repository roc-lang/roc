interface Rbt
  exposes [ Rbt, Job, job ]
  imports []

Job : [ Job [Command] (List Job) ]

# TODO: these fields are all required until https://github.com/rtfeldman/roc/issues/1844 is fixed
job : [Command], List Job -> Job
job = Job

Rbt : [ Rbt Job ]
