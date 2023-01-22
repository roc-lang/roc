# Arena Allocator Example

This is an example of custom memory management in Roc. It uses arena allocation, which allows a collection of objects (an "arena") to be freed all at once. In this example, `Task.useArenaAlloc` causes a task to run like this:

1. An arena is created, and it's used as the current allocator.
2. The task is run.
   * When the task requests a memory allocation, it's allocated in the arena.
   * When the task requests memory to be freed, nothing happens.
3. The arena is freed, causing the memory used by the task to be freed all at once.
4. The previous allocator is used as the current allocator. It could be another arena allocator if `useArenaAlloc` is nested. Otherwise, it uses the same allocation used by other examples.

The program in this example asks for a string, then it outputs the string's bytes in decimal notation.
