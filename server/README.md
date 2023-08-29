# server

Run this on a machine with cmus, plugged into your audio system. Expects cmus to already be running; performs very little error handling against the outside world.

Build it with `stack build` and move/name the binary to where you please. Probably want a name other than `server`.

Run it with `server n`, where `n` is your desired port number.

It exposes the following routes. Everything is a GET method to avoid CORS nonsense. This is only intended for local use so who gives a fuck.

| Route         | Payload                                                                                                  |
|---------------|----------------------------------------------------------------------------------------------------------|
|/              |Main app view. The only HTML response here.                                                               |
|/sync          |Tree view of library and current queue. Run at startup and every time new files are added to cmus library.|
|/queue         |Current queue.                                                                                            |
|/play          |Toggles play/pause state of cmus. Always returns 200.                                                     |
|/vol/{n}       |Sets playback volume to n ∈ [0, 100]. Requires `set softvol=true` in cmus. Returns new volume for sync.   |
|/add/{n}-{m}...|Adds track numbers [n, m, ...] to queue. Returns queue for sync.                                          |
|/remove/{n}    |Removes track {n} from queue. Returns queue for sync.                                                     |

