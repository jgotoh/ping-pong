Small application to test Cloud Haskell's process registry and search mechanism

You can either start a process and wait for a message:

`cabal new-run exes -- --startip localhost --startp 3000`

The final EndpointAddress where your process is registered is then printed to the console, e.g `localhost:3000:0`.

Or you can join a already started process:

`cabal new-run exes -- --ip localhost --p 3001 --j localhost:3000:0`

