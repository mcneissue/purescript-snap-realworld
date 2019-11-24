## Running the Tests

Run all tests: `spago test`

### API Test Config

The API tests require a config file `tests/config.json` with the following format:

```javascript
{
  "startServer": {
    "executable": "", // Server executable to spawn before each test
    "arguments": [""] // string arguments passed to the executable
  },
  "workingDir": "",   // Working directory for cleanup and startServer commands
  "apiUrl": "",       // Root URL of the api, e.g. http://localhost:3000/api
  "cleanup": "",      // Cleanup command to run after each test
  "envVars": [        // Env vars for the startServer and cleanup commands
    { "key": "", "value": "" } 
  ]
}
```

The `cleanup` command should clear out the database.