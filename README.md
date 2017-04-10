Simple multiplayer haskell game.

TODO list:
- [x] Refactor all files (complete all todos, fine-grain imports, etc.)
- [x] Change how outgoing messages are processed on the server
      * Have one thread per client which monitors its out TChan
- [ ] Add basic logging (only on networking)
- [ ] Initial handshake between server and client