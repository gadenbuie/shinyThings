This is a small demonstration of the `dropdownButton` module. A global `button_options` contains the labels (as names) and input IDs for the dropdown button elements.

### UI

The UI element is created with `dropdownButtonUI()`. The `id` parameter links the dropdown button UI to the dropdown module on the server side. The `label` parameter specifies the text on the "master" button. The button can be either a `"dropdown"` or `"dropup"` button, set with `type`. Finally, additional classes for and the id of the dropdown button element can be specified with `class` and `buttonId`, respectively.

### Server

On the server side, the Shiny module is created with

```r
last_clicked <- callModule(dropdownButton, id, options = button_options)
```

where both `id` and `options` need to match the values of those arguments to `dropdownButtonUI()`. The module returns the value of the most recently selected button, updated on each button click. The returned value is a reactive that needs to be referenced within a reactive context.
