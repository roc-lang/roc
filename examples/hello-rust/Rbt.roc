interface Rbt
  exposes [ Job, job ]
  imports []

Job : [ Job [Command] (List Job) ]

job : [Command], List Job -> Job
job = Job
