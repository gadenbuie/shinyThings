function toggleHistoryButtonState (state) {
  if (state.enable) {
    for (var i = 0; i < state.enable.length; i++) {
      var btn_e = $('#' + state.enable[i]);
      btn_e.prop('disabled', false).removeClass('disabled');
    }
  }
  if (state.disable) {
    for (var j = 0; j < state.disable.length; j++) {
      var btn_d = $('#' + state.disable[j]);
      btn_d.prop('disabled', true).addClass('disabled');
    }
  }
}

Shiny.addCustomMessageHandler('undoHistoryButtons', toggleHistoryButtonState);
