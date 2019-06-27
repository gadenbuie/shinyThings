$(document).on("click", ".shinythings-btn-group > .btn", function(evt) {
  // button that was clicked
  var el = $(evt.target).closest('button');

  // toggle state of clicked button
  if (el.hasClass('active')) {
    el.removeClass('active');
  } else {
    el.addClass('active');
    if (!parseInt(el.parent().attr('data-multiple'))) {
      // deactive other buttons if only one active button allowed
      el.siblings().removeClass('active');
    }
  }

  // remove focus from button
  el.blur();

  // Raise event to signal value has changed
  el.trigger("change");
});

var shinythingsGroupBinding = new Shiny.InputBinding();

$.extend(shinythingsGroupBinding, {
  find: (scope) => scope.querySelectorAll(".shinythings-btn-group[id]"),
  //getType: (el) => "shinythings.buttonGroup",
  getValue: function(el) {
    let value = $(el)
      .find(".active")
      .map(function() {return this.value})
      .get();

    if (value.length > 0) {
      return value;
    } else {
      return null;
    }
  },
  setValue: function(el, value) {
    el.children().removeClass('active');
    if (value.length) {
      for (let val_id of value) {
        el.find("#" + el.prop('id') + '__' + val_id).addClass('active');
      }
    }
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

Shiny.inputBindings.register(shinythingsGroupBinding, 'shinythings.buttonGroup');
