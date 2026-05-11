# Workflow Agent CLI Lives Outside Dotfiles

The Workflow Agent CLI source will live in a standalone Agent Source Project rather than inside this dotfiles repository. The agent has product-shaped concerns such as OAuth, provider transports, storage, tools, tests, and later remote execution, so dotfiles should configure and launch it while the CLI itself evolves as a separate package.
