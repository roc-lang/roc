interface Task
    exposes [Task, stdoutLine]
    imports [InternalTask]

Task ok err := InternalTask.Task ok err

stdoutLine : Str -> Task {} *
stdoutLine = \line -> @Task (InternalTask.stdoutLine line)
