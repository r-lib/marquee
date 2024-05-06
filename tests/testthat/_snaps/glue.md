# glue properly detects custom spans

    Failed to parse glue component
    Caused by error in `parse()`:
    ! <text>:1:5: unexpected '{'
    1: .red{
            ^

---

    Failed to evaluate glue component {.red}
    Caused by error:
    ! object '.red' not found

---

    Malformed marquee interpolation block. When interpolating a tag name it must be followed by a space

---

    evaluated tag ("tag 2") is not a valid tag name

