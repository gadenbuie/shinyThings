This is a small demonstration of the `radioSwitchButtons` input.

### UI

The `radioSwitchButtons()` input operates exactly like `shiny::radioButtons()`
but the radio labels are styled to look like buttons. The input takes a vector
of choices and their labels; vector names are used for button labels and the
values are returned by the input as a character vector when a button is toggled.

Because the radio buttons only *look like* buttons, they cannot inherit the
colors from the styles of the rest of your app. In other words, you will need
to manually match the button colors to your app using the `selected_background`
and `selected_color` options (and the corresponding arguments for 
`not_selected`). `shinyThings` also provides a helper function
`radioSwitchButtons_default_style()` that will globally set the default button
colors for you. You can still override those color choices for particular 
inputs.
