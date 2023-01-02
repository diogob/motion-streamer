# motion-streamer

## Quickstart

To run on Raspberry OS (Bullseye):

```sh
apt install libgirepository1.0-dev libgstreamer1.0-dev llvm libnuma-dev gstreamer1.0-opencv
cabal run motion-streamer
```

In case you see the error `motion-streamer: FactoryError` it is probably due to the lack of a `libcamerasrc` element in your gstreamer instalation.
To install libcamera from sources check their [getting started section](https://libcamera.org/getting-started.html)

You can still run without libcamera using a test pattern:

```sh
MS_TEST=true cabal run motion-streamer
```
