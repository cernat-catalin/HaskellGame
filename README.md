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
- [x] Client disconnects
- [ ] Client architecture
- [x] OpenGL context on client
- [x] OpenGL basic rendering
- [x] GLFW input events
- [x] Handle disconnect from client (normal and forced)
- [ ] Server services
- [ ] Server main loop
- [ ] Simple playable game
- [ ] Replace map with an unordered map
- [ ] Lenses
- [ ] Separate messages
- [ ] Code review
- [ ] Review messages structure
- [ ] Review pure STM functions like addClient