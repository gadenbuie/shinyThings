"use strict";

$(document).on("click", ".shinythings-radio-inputs > input", function(event) {
  var el = $(event.target).closest(".shinythings-radio-inputs");
  el.trigger("change");
});

var shinythingsRadioSwitchBinding = new Shiny.InputBinding();

$.extend(shinythingsRadioSwitchBinding, {
  find: function find(scope) {
    return $(scope).find(".shinythings-radio-inputs[id]");
  },
  getValue: function getValue(el) {
    var value = $(el).find("input:checked").val();

    if (value.length > 0) {
      return value;
    } else {
      return null;
    }
  },
  setValue: function(el, value) {
    var $el = $(el);
    $el.find("input[type='radio']").prop("checked", false);
    if (typeof value === "string") {
      $el.find("input[value=\"" + value + "\"]").prop("checked", true);
    } else {
      console.log("Unable to process value: " + value)
    }
    $el.closest(".shinythings-radio-inputs").trigger("change")
  },
  subscribe: function(el, callback) {
    $(el).on("change.shinythingsRadioSwitchBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".shinythingsRadioSwitchBinding");
  },
  receiveMessage: function(el, msg) {
    if (msg.value) {
      shinythingsRadioSwitchBinding.setValue(el, msg.value);
    }
  }
});

Shiny.inputBindings.register(shinythingsRadioSwitchBinding, 'shinythings.radioSwitchButton');
