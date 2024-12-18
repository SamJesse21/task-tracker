import { describe, it, beforeEach, expect } from 'vitest';

// Mocking the Task Tracker smart contract for testing purposes
const mockTaskTracker = {
  state: {
    tasks: {} as Record<number, any>, // Maps task IDs to task details
    nextTaskId: 0, // Keeps track of the next task ID
    taskIds: [] as number[], // List of task IDs
  },

  // Mocking the create-task function
  createTask: (title: string, description: string | null, deadline: number, caller: string) => {
    const taskId = mockTaskTracker.state.nextTaskId;
    if (mockTaskTracker.state.taskIds.length >= 1000) {
      return { error: 500 }; // Maximum task limit reached
    }

    mockTaskTracker.state.tasks[taskId] = {
      title,
      description,
      deadline,
      completed: false,
      creator: caller,
    };

    mockTaskTracker.state.taskIds.push(taskId);
    mockTaskTracker.state.nextTaskId += 1;

    return { value: taskId };
  },

  // Mocking the complete-task function
  completeTask: (taskId: number, caller: string) => {
    const task = mockTaskTracker.state.tasks[taskId];
    if (!task) return { error: 404 }; // Task not found

    if (task.creator !== caller) {
      return { error: 403 }; // Only the creator can complete the task
    }

    task.completed = true;
    return { value: true };
  },

  // Mocking the get-task function
  getTask: (taskId: number) => {
    return mockTaskTracker.state.tasks[taskId] || null;
  },

  // Mocking the get-user-tasks function
  getUserTasks: (caller: string) => {
    return mockTaskTracker.state.taskIds.filter((taskId) => {
      const task = mockTaskTracker.state.tasks[taskId];
      return task && task.creator === caller;
    });
  },
};

describe('Task Tracker Contract', () => {
  let user1: string, user2: string;

  beforeEach(() => {
    // Initialize mock state and user principals
    user1 = 'ST1234...';
    user2 = 'ST5678...';

    mockTaskTracker.state = {
      tasks: {},
      nextTaskId: 0,
      taskIds: [],
    };
  });

  it('should allow a user to create a task', () => {
    const result = mockTaskTracker.createTask('Sample Task', 'This is a task description', 1234567890, user1);
    expect(result).toEqual({ value: 0 });
    expect(mockTaskTracker.state.tasks[0].title).toBe('Sample Task');
    expect(mockTaskTracker.state.tasks[0].creator).toBe(user1);
  });

  it('should prevent task creation if the task limit is reached', () => {
    // Fill the task list to the limit
    for (let i = 0; i < 1000; i++) {
      mockTaskTracker.createTask(`Task ${i}`, null, 1234567890, user1);
    }

    const result = mockTaskTracker.createTask('New Task', null, 1234567890, user1);
    expect(result).toEqual({ error: 500 });
  });

  it('should allow a user to mark a task as completed', () => {
    // Create a task
    mockTaskTracker.createTask('Task 1', 'Description 1', 1234567890, user1);
    const taskId = 0;

    const result = mockTaskTracker.completeTask(taskId, user1);
    expect(result).toEqual({ value: true });
    expect(mockTaskTracker.state.tasks[taskId].completed).toBe(true);
  });

  it('should prevent non-creators from marking a task as completed', () => {
    // Create a task
    mockTaskTracker.createTask('Task 1', 'Description 1', 1234567890, user1);
    const taskId = 0;

    const result = mockTaskTracker.completeTask(taskId, user2);
    expect(result).toEqual({ error: 403 });
  });

  it('should retrieve the correct task details', () => {
    // Create a task
    mockTaskTracker.createTask('Task 1', 'Description 1', 1234567890, user1);
    const taskId = 0;

    const task = mockTaskTracker.getTask(taskId);
    expect(task).toBeTruthy();
    expect(task.title).toBe('Task 1');
  });

  it('should return null for non-existing tasks', () => {
    const task = mockTaskTracker.getTask(9999);
    expect(task).toBeNull();
  });

  it('should list tasks created by the caller', () => {
    // Create tasks for both users
    mockTaskTracker.createTask('Task 1', 'Description 1', 1234567890, user1);
    mockTaskTracker.createTask('Task 2', 'Description 2', 1234567890, user2);
    mockTaskTracker.createTask('Task 3', 'Description 3', 1234567890, user1);

    const user1Tasks = mockTaskTracker.getUserTasks(user1);
    const user2Tasks = mockTaskTracker.getUserTasks(user2);

    expect(user1Tasks).toEqual([0, 2]); // user1 should have tasks with IDs 0 and 2
    expect(user2Tasks).toEqual([1]); // user2 should have task with ID 1
  });
});
