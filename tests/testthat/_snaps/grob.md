# grobs gets correctly constructed

    `hjust` must be a valid justification
    i Use either numerics or one of "left", "left-ink", "center", "center-ink", "right-ink", or "right"

---

    `hjust` must either be numeric or a character vector

---

    `vjust` must be a valid justification
    i Use either a numerics or one of "bottom", "bottom-ink", "last-line", "center", "center-ink", "first-line", "top-ink", or "top"

---

    `hjust` must either be numeric or a character vector

---

    Code
      grob1$blocks
    Output
      $length
       [1]  1  1 15  3  1  1  1  1  1  1  3  1  7  1  1  1  1  1  1  1  1  1  1  1  1
      [26]  1  1  1  1  1  1  5
      
      $start
       [1]  1  2  3 18 21 22 23 24 25 26 27 30 31 38 39 40 41 42 43 44 45 46 47 48 49
      [26] 50 51 52 53 54 55 56
      
      $indent
       [1] 1 2 2 2 2 3 3 4 4 3 2 2 2 2 2 3 4 3 4 4 5 6 6 7 8 8 6 7 3 4 2 2
      

---

    Code
      grob1$bullets
    Output
      $bullet
      [1] "1." "2." "1." "2." "3." "•"  "1."
      
      $index
      [1] 24 25 40 42 53 45 48
      
      $placement
      [1] 24 25 41 43 54 46 49
      
      $shape
      $shape$shape
      # A tibble: 13 x 14
         glyph index metric_id string_id x_offset y_offset font_path        font_index
         <int> <int>     <int>     <int>    <dbl>    <dbl> <chr>                 <int>
       1     1    20         1         1   -10.0     -9.24 /System/Library~          0
       2     2    17         1         1    -3.33    -9.24 /System/Library~          0
       3     1    21         2         2   -10.0     -9.24 /System/Library~          0
       4     2    17         2         2    -3.33    -9.24 /System/Library~          0
       5     1    20         3         3   -10.0     -9.24 /System/Library~          0
       6     2    17         3         3    -3.33    -9.24 /System/Library~          0
       7     1    21         4         4   -10.0     -9.24 /System/Library~          0
       8     2    17         4         4    -3.33    -9.24 /System/Library~          0
       9     1    22         5         5   -10.0     -9.24 /System/Library~          0
      10     2    17         5         5    -3.33    -9.24 /System/Library~          0
      11     1   135         6         6    -4.20    -9.24 /System/Library~          0
      12     1    20         7         7   -10.0     -9.24 /System/Library~          0
      13     2    17         7         7    -3.33    -9.24 /System/Library~          0
      # i 6 more variables: font_size <dbl>, advance <dbl>, ascender <dbl>,
      #   descender <dbl>, col <chr>, id <int>
      
      $shape$metrics
      # A tibble: 7 x 11
        string width height left_bearing right_bearing top_bearing bottom_bearing
        <chr>  <dbl>  <dbl>        <dbl>         <dbl>       <dbl>          <dbl>
      1 1.     10.0      12        1.15          1.08        0.885           2.76
      2 2.     10.0      12        0.375         1.08        0.819           2.76
      3 1.     10.0      12        1.15          1.08        0.885           2.76
      4 2.     10.0      12        0.375         1.08        0.819           2.76
      5 3.     10.0      12        0.287         1.08        0.838           2.53
      6 •       4.20     12        0.874         0.739       3.65            2.76
      7 1.     10.0      12        1.15          1.08        0.885           2.76
      # i 4 more variables: left_border <dbl>, top_border <dbl>, pen_x <dbl>,
      #   pen_y <dbl>
      
      

