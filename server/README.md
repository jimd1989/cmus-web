# server

Run this on a machine with cmus, plugged into your audio system. Expects cmus to already be running; performs very little error handling against the outside world.

Build it with `stack build` and move/name the binary to where you please. Probably want a name other than `server`.

Run it with `server n`, where `n` is your desired port number.

It exposes the following routes:

| Route         | Method | Payload                                                                                                  |
|---------------|--------|----------------------------------------------------------------------------------------------------------|
|/              |GET     |Tree view of library and current queue. Empty until synced.                                               |
|/sync          |GET     |Tree view of library and current queue. Run at startup and every time new files are added to cmus library.|
|/queue         |GET     |Current queue.                                                                                            |
|/play          |PUT     |Toggles play/pause state of cmus. Always returns 200.                                                     |
|/add/{n}-{m}...|POST    |Adds track numbers [n, m, ...] to queue. Empty response for performance reasons.                          |
|/remove/{n}    |DELETE  |Removes track {n} from queue. Returns current queue, which is less than ideal, but ensures sync.          |

