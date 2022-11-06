# Project description

Ada PNG modifier is a short project to improve my skills in Ada and its GPR build system. 
Objective is to modify a PNG image, by inversing Red, Green and Blue values for example.

Currently, tests are not yet implemented. Corner cases have not been tested.

# Building 

## Dependencies:

Project needs gprbuild to build.

## Commands

Build the project:
```
gprbuild
```

Clean:
```
gprclean
```

# Using it

Help:
```
./obj/main -h
```

Example:
```
./obj/main -i ./images/blackbuck.ascii.ppm -o ./images/blackbuck.ascii.modified.ppm
```

## Results:

Images weight too much to be displayed on web browsers, you need to click on it.

![Initial image](./images/blackbuck.ascii.ppm "Initial image")
![Modified image](./images/blackbuck.ascii.modified.ppm "Modified image")

# TODO

- [ ] Improve encapsulations
- [ ] Parallelize pixel processing (maybe with OpenMP ?)
- [ ] Add Unit tests and system tests
- [ ] Support more formats
- [ ] Improve exception handling
- [ ] Replace remaining magic numbers
