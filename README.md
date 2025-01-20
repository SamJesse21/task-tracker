# task-tracker
 

# Task Tracker Smart Contract 📋

**Description:**  
The Task Tracker Smart Contract is designed to help users manage and track tasks on the blockchain. It allows users to create tasks, mark them as completed, and retrieve details of tasks. This contract ensures that tasks are organized, deadlines are met, and only task creators can mark tasks as completed. Perfect for managing personal or team projects in a decentralized way!

---

## Features 🚀  

- **Create a Task:**  
  Users can create tasks with a title, optional description, and a deadline. Tasks are automatically assigned a unique ID and stored securely on the blockchain.

- **Mark Task as Completed:**  
  Once a task is finished, only the creator of the task can mark it as completed.

- **Get Task Details:**  
  Retrieve the title, description, deadline, and status of any task.

- **List User's Tasks:**  
  Retrieve a list of tasks created by the current user, helping to keep track of their progress.

- **Task Limit:**  
  The system is designed to handle a maximum of 1000 tasks. Any attempt to create more than this limit will be rejected.

---

## Contract Functions 📜

### Public Functions  

#### `create-task`  
**Parameters:**  
- `title (string-utf8 100)`: The title of the task.  
- `description (optional (string-utf8 500))`: An optional description of the task.  
- `deadline (uint)`: The deadline of the task in UNIX timestamp format.  

**Behavior:**  
- Creates a new task with the provided title, description, and deadline.  
- The task is assigned a unique ID and added to the contract's state.  
- Only 1000 tasks are allowed in total; attempts to exceed this limit will return an error.  

**Returns:**  
- `task-id (uint)`: The unique ID of the created task.  
- `err u500`: If the task limit has been reached.  

---

#### `complete-task`  
**Parameters:**  
- `task-id (uint)`: The ID of the task to be marked as completed.  

**Behavior:**  
- Only the creator of the task can mark it as completed.  
- If the task exists and the sender is the creator, the task’s completion status is updated.  

**Returns:**  
- `ok true`: If the task was successfully marked as completed.  
- `err u403`: If the caller is not the creator of the task.  
- `err u404`: If the task does not exist.  

---

#### `get-task`  
**Parameters:**  
- `task-id (uint)`: The ID of the task to retrieve.  

**Returns:**  
- A task object containing:  
  - `title`: The title of the task.  
  - `description`: The description of the task (if provided).  
  - `deadline`: The deadline of the task.  
  - `completed`: A boolean indicating if the task is completed.  
  - `creator`: The principal address of the creator.  
- `null`: If the task does not exist.  

---

#### `get-user-tasks`  
**Returns:**  
- A list of task IDs created by the current user.  

---

## Unit Tests 🧪  

Unit tests have been implemented to validate the contract functionality. These tests ensure that the contract operates as expected, including edge cases.

### Test Cases:

1. **Create Task:**  
   Users can create a task with a valid title, description, and deadline. The task ID is returned and stored correctly.

2. **Task Limit Reached:**  
   The contract prevents new tasks from being created once the limit of 1000 tasks is reached, returning an error.

3. **Complete Task:**  
   Only the creator of a task can mark it as completed. Unauthorized users are prevented from completing tasks.

4. **Get Task Details:**  
   Tasks can be retrieved by their ID, and the correct details are returned.

5. **Get User Tasks:**  
   Users can retrieve a list of tasks they have created, showing all relevant task IDs.

---

## Example Usage 📝

### Create a Task:
```clarity
(create-task "My First Task" "This is a task description" 1234567890)
```

### Complete a Task:
```clarity
(complete-task 0)
```

### Get Task Details:
```clarity
(get-task 0)
```

### Get User's Tasks:
```clarity
(get-user-tasks)
```

---

## Deployment 🚀  

To deploy the Task Tracker contract:
1. Deploy the contract on the desired blockchain network.  
2. Ensure users can interact with the contract through their wallet interfaces.  
3. Run the unit tests to verify that all functions are working as expected.

---

## License 📄  

This project is open-source and available under the MIT License.
