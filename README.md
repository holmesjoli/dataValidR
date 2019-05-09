# Data Valid R

<!-- badges: start -->
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/5c55b64f4df6402a97fb8194e888a5d7)](https://www.codacy.com/app/holmesjoli/dataValidR?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=holmesjoli/dataValidR&amp;utm_campaign=Badge_Grade)
[![Codecov test coverage](https://codecov.io/gh/holmesjoli/dataValidR/branch/master/graph/badge.svg)](https://codecov.io/gh/holmesjoli/dataValidR?branch=master)

<!-- badges: end -->

The data valid r package is a series of tests used for data validation.

## Tests

### Column-level tests

#### General Tests

-   **test_unique**: tests the uniqueness of a dataframe column/series
-   **test_values**: tests the values of a dataframe column/series against the expected values
-   **test_null_values**: tests that there are no null values in adataframe column/series
-   **test_type**: tests that the type is equal to the expected column type. Expected type can take on the following values: _char_string_, _string_, _str_, _number_, _numeric_, _integer_, _int_, _float_, _flt_, _double_, _complex_, _boolean_, _bool_, _datetime_, _date_.

#### Boolean Tests

-   **test_all_true**: tests all values of a dataframe column/series are true
-   **test_all_false**: tests all values of a dataframe column/series are false
-   **test_any_true**: tests some values of a dataframe column/series are true
-   **test_any_false**: tests some values of a dataframe column/series are false

#### Numeric tests

-   **test_less_than_value**: tests all values in a dataframe column/series are less than the specified upper bound
-   **test_less_than_or_equal_value**: tests all values in a dataframe column/series are less than or equal to the specified upper bound
-   **test_greater_than_value**: tests all values in a dataframe column/series are greater than the specified lower bound
-   **test_greater_than_or_equal_value**: tests all values in a dataframe column/series are greater than or equal to the specified lower bound
-   **test_inclu_value_range**: tests all values are in a dataframe column/series are less than or equal to the upper bound and greater than or equal to the lower bound
-   **test_exclu_value_range**: tests all values in a dataframe column/series are less than the upper bound and greater than the lower bound
-   **test_exclu_lower_inclu_upper_range**: tests that all values in a dataframe column/series are less than or equal to the upper bound and greater than the lower bound
-   **test_inclu_lower_exclu_upper_range**: tests that all values in a dataframe column/series are less than the upper bound or greater than or equal to the lower bound
