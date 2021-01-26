# grainchanger 0.3.1
* Add GitHub actions for testing

# grainchanger 0.3.0
* Fix failing test on linux

# grainchanger 0.2.0

* successful review through rOpenSci
* added in functionality to aggregate data to polygon (use parameter `is_grid = FALSE`) 
* added in ability for user to specify aggregation function (`agg_fun`) in `winmove_agg`
* note that many parameter names have changed - check docs for more info: 
    * `fn` is now `win_fun`
    * `g` is now `coarse_dat`
    * `dat` is now `fine_dat`
* `agg_fun` and `win_fun` should be entered as unquoted function names (previously character input)

# grainchanger 0.1.0

* First stable release of grainchanger 

# grainchanger 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
