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
- [x] Client architecture
- [x] OpenGL context on client
- [x] OpenGL basic rendering
- [x] GLFW input events
- [x] Handle disconnect from client (normal and forced)
- [x] Server services
- [x] Separate messages
- [x] Review messages structure (so that patterns are exhaustive both on server and client)
- [ ] Handle client input process
- [ ] Separate files in the Common folder
- [ ] Code review
- [ ] Server main loop
- [ ] Simple playable game
- [ ] Replace map with an unordered map
- [ ] Lenses
- [ ] Standardize log and error messages
- [ ] Code review (also review pure STM functions like addClient)