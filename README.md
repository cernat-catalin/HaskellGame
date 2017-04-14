Simple multiplayer haskell game.

TODO list:
- [x] Refactor all files (complete all todos, fine-grain imports, etc.)
- [x] Change how outgoing messages are processed on the server
      * Have one thread per client which monitors its out TChan
- [x] Add basic logging (only on networking)
- [x] Initial handshake between server and client
- [x] Redo the architecture (HTA - will describe later)
- [ ] Basic game objects
- [ ] Baisc transformations for game objects
- [ ] OpenGL context on client
- [ ] Server main loop
- [ ] Simple playable game
- [ ] Code review