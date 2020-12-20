# watch-together
Minimal KISS web app to watch movies together.

## Build from Source
In order to compile this project you need [stack](http://haskellstack.org).

```bash
  git clone https://github.com/fmeyer3141/watch-together
  cd watch-together
  stack build
```

## Usage
Once the project has successfully been compiled run:

```bash
  stack exec -- watch-together-exe /path/to/video/file 3000
```

This will start a webserver on port 3000.
