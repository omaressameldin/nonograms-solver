# README #
### Project Info ###
* Nonograms solver

### Description ###
* A nonograms solver using prolog

### What are nonograms ###
* Nonograms, are picture logic puzzles in which cells in a grid must be colored or left blank according to numbers at the side of the grid to reveal a hidden picture. In this puzzle type, the numbers are a form of discrete tomography that measures how many unbroken lines of filled-in squares there are in any given row or column. For example, a clue of "4 8 3" would mean there are sets of four, eight, and three filled squares, in that order, with at least one blank square between successive groups. (from wikipedia)

### How to run ###
* pull repo
  * `git init`
  * `git remote add origin [url]`
  * `git pull origin master`

* build app:
  * `docker-compose up --build`

* run prolog
  * `docker exec -it nonograms-solver swipl`

* load program
  * `[nonogram].`
* open `examples.pl` file and choose one of them and paste it
  * eg: `solve([[1], [3], [1,2], [1,3], [2,4], [7], [1], [10], [8]],[[1], [2,2], [1,2,2], [1,1,2], [9], [5,2], [3,2], [2,2], [1,2], [1]], Boat).`