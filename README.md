Simple multiplayer haskell game.

TODO list:
- [x] Refactor all files (complete all todos, fine-grain imports, etc.)
- [x] Change how outgoing messages are processed on the server
      * Have one thread per client which monitors its out TChan
- [x] Add basic logging (only on networking)
- [x] Two modes: with logging and without
- [x] Initial handshake between server and client
- [x] Redo the architecture (HTA - will describe later)
- [x] Basic game objects
- [x] Baisc transformations for game objects
- [ ] Client disconnects
- [ ] Client arcihtecture
- [x] OpenGL context on client
- [x] OpenGL basic rendering
- [x] GLFW input events
- [ ] Handle disconnect from client (normal and forced)
- [ ] Server main loop
- [ ] Simple playable game
- [ ] Replace map with an unordered map
- [ ] Separate messages
- [ ] Lenses
- [ ] Code review