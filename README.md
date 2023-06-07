# Scrobburl

A multiplayer word game, where all state is stored in the URL (no backend needed!).
The twist? The game state is (will be) encoded so that _no player can cheat_ (modify state illegally, or predict/manipulate RNG).

Scrobburl is in **very early development**, and most features aren't implemented yet. This includes the URL encoding, but I do have a plan for how to do it.

## Development

To run the app for development, open the dev container with VSCode, then press `ctrl+shift+P` and search for "Run build task". This will start the app on port 8000. The build task is defined in `./vscode/tasks.json`.

Notes:
- To make the dev server accessible from another device on the same network, change the [Local Port Host](https://stackoverflow.com/a/67997839) setting in VSCode to `allinterfaces`.
- If you're using Windows, make sure to clone the repo into your WSL file system, otherwise the elm-live file watcher won't work. Alternatively, you can probably do everything in Windows directly, but I haven't tested that.