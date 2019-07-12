This is a small demonstration of the `buttonGroup` input. 

### UI

The `buttonGroup()` input operates either like `shiny::radioButtons()` or
`shiny::checkboxGroupInput()`. The input takes a vector of button labels; the 
vector names are used for button labels and the values are returned by the input
as a character vector when a button is toggled.

When using `multiple = FALSE` (the default), only one button is allowed to be
toggled at a time and returns a length one character string. Setting 
`multiple = TRUE` allows more than one button to be toggled and returns a vector
of values corresponding to the toggled buttons. When no button is selected and
empty character string is returned.
