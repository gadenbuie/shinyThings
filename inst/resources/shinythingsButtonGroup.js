$(document).on("click", ".shinythings-btn-group > .btn", function(evt) {
  // button that was clicked
  var el = $(evt.target).closest('button');

  // set state of clicked button to active and deactivate siblings
  // and set the data-active attribute of parent div
  if (el.hasClass('active')) {
    el.removeClass('active');
    //el.parent().attr('data-active', '');
  } else {
    el.addClass('active');
    if (!parseInt(el.parent().attr('data-multiple'))) {
      el.siblings().removeClass('active');
    }
  }
  el.blur();

  setGroupButtonDataActive(el.closest('.shinythings-btn-group').prop('id'));
});

const setGroupButtonDataActive = function(id) {
  var $el = $('#' + id);
  var btn_active = $el
    .find(".active")
    .map(function() { return this.id.replace($el.prop('id') + '__', '') })
    .get()

  if (btn_active.length === 0) {
    $el.attr('data-active', '');
  } else {
    $el.attr('data-active', JSON.stringify(btn_active));
  }
  // Raise event to signal value has changed
  $el.trigger("change");
}

var shinythingsGroupBinding = new Shiny.InputBinding();
$.extend(shinythingsGroupBinding, {
  find: function(scope) {
    return $(scope).find(".shinythings-btn-group");
  },
  initialize: function(el) {
    setGroupButtonDataActive(el.id);
  },
  getValue: function(el) {
    var btn_active = $(el).attr('data-active');
    if (btn_active) {
      return JSON.parse(btn_active);
    } else {
      return btn_active;
    }
  },
  setValue: function(el, value) {
    el.children().removeClass('active');
    if (value.length) {
      for (let val_id of value) {
        el.find("#" + el.prop('id') + '__' + val_id).addClass('active');
      }
    }
    setGroupButtonDataActive(el.prop('id'));
  },
  subscribe: function(el, callback) {
    $(el).on("change.shinythingsGroupBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".shinythingsGroupBinding");
  }
});

Shiny.inputBindings.register(shinythingsGroupBinding, 'shinythings.buttonGroupBinding');
