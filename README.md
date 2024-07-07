# Scrobburl

A multiplayer word game where all state is stored in the URL (no backend needed!).
Just send a link to your opponent after each turn.

## Why store state in the URL?

Aside from doing it for the sake of curiosity, there are some practical benefits to doing this versus having a dedicated server:
- It's easier to host - the entire game is just some static files, so it can be hosted easily on a static hosting platform (I'm using GitHub Pages) without needing to run any code on the backend or use a database.
- Users don't need to log in or create accounts.
- It's decentralized - use whatever mechanism you want to send links to your opponents (email, SMS, etc).

## How do you prevent cheating?

Right now, the implementation doesn't do a whole lot to prevent cheating in the form of RNG prediction/manipulation.
However, it's not possible for a player to cheat by modifying the score counters or playing illegal moves. This is because the game state is actually stored as a _list of all turns so far_, so the opponent can easily re-compute the final state and verify that all moves were valid.

My original idea for this project was to take it a step further, and use cryptographic techniques to ensure that players can't view their opponents tiles or predict what tiles they'll get next.
I haven't gotten around to actually implementing this yet though 😃.

## Development

To run the app for development, open the dev container with VSCode, then press `ctrl+shift+P` and search for "Run build task". This will start the app on port 8000. The build task is defined in `./vscode/tasks.json`.

Notes:
- To make the dev server accessible from another device on the same network, change the [Local Port Host](https://stackoverflow.com/a/67997839) setting in VSCode to `allinterfaces`.
- If you're using Windows, make sure to clone the repo into your WSL file system, otherwise the elm-live file watcher won't work. Alternatively, you can probably do everything in Windows directly, but I haven't tested that.