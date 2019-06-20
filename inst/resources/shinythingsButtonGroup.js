$(document).on("click", ".shinythings-btn-group > .btn", function(evt) {
  // button that was clicked
  var el = $(evt.target);

  // set state of clicked button to active and deactivate siblings
  // and set the data-active attribute of parent div
  if (el.hasClass('active')) {
    el.removeClass('active');
    //el.parent().attr('data-active', '');
  } else {
    el.addClass('active');
    if (!parseInt(el.parent().attr('data-multiple'))) {
      el.siblings().removeClass('active');
    };
  };
  el.blur();

  var btn_active = el.parent()
    .find(".active")
    .map(function() { return this.id })
    .get()

  if (btn_active.length === 0) {
    el.parent().attr('data-active', '');
  } else {
    btn_active = btn_active.reduce(function(x, y) {return x + '", "' + y});

    el.parent().attr('data-active', '["' + btn_active + '"]');
  }

  // Raise event to signal value has changed
  el.parent().trigger("change");
});

var shinythingsGroupBinding = new Shiny.InputBinding();
$.extend(shinythingsGroupBinding, {
  find: function(scope) {
    return $(scope).find(".shinythings-btn-group");
  },
  getValue: function(el) {
    var btn_active = $(el).attr('data-active');
    if (btn_active) {
      return JSON.parse(btn_active);
    } else {
      return btn_active;
    };
  },
  setValue: function(el, value) {
    $(el).attr('data-active', value);
    $(el).find(value).addClass('active').siblings().removeClass('active');
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
