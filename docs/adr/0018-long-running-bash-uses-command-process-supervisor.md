# Long-Running Bash Uses Command Process Supervisor

Long-running `bash` execution will use a dedicated Command Process Supervisor rather than the Ambient Agent Run model. Ambient runs represent agent conversations and follow-up executions; supervised command processes represent OS commands with status, tail, stdin, and signal controls. The two surfaces should share inspection and canonical event patterns, but their identities and lifecycles stay separate.
