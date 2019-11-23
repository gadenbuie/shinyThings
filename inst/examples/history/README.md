This is a small demonstration of the `undoHistory` module.

This is a simple Shiny module for undo/redo history. The Shiny module accepts
an arbitrary reactive data value. Changes in the state of this reactive value
are tracked and added to the user's history. The user can then repeatedly
undo and redo to walk through this stack. The module returns the currrent
selected value of the reactive from this historical stack, or `NULL` when
the app state was changed by the user. 

### UI

The UI undo/redo buttons are created with `undoHistoryUI()`. The `id` parameter links the undo/redo history buttons in the UI to the undo/redo history module on the server side. You can customize the appearance of the the `back` buttons and the `fwd` buttons with the `_icon`, `_text`, and `_class` arguments prefixed with `back` or `fwd`.

### Server

On the server side, the undo/redo Shiny module is created with

``` r
undo_app_state <- undoHistory("id", value = reactive({
  # reactive inputs for historical tracking
  input$text
}))
```

When the user is modifying the app state, the module returns `NULL`. When the user is navigating the undo/redo history, the module returns the value of the most recently selected historical state. 

Because this reactive can hold arbitrary data about the state of the Shiny app, it is up to the app developer to use the returned current value to update the Shiny apps' inputs and UI elements.

``` r
observe({
  req(!is.null(undo_app_state()))
  updateTextInput("text", value = undo_app_state())
})
```
