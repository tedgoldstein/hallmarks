// Put code in an Immediately Invoked Function Expression (IIFE).
// This isn't strictly necessary, but it's good JavaScript hygiene.
(function() {

// See http://rstudio.github.io/shiny/tutorial/#building-outputs for
// more information on creating output bindings.

// First create a generic output binding instance, then overwrite
// specific methods whose behavior we want to change.
var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".hallmark-chart");
};

binding.renderValue = function(el, data) {
  // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
  

  var $el = $(el);
    
  // The first time we render a value for a particular element, we
  // need to initialize the d3 selection. We'll
  // store these on $el as a data value called "state".
  if (!$el.data("state")) {
    var selection = d3.select(el).select("svg");
    
    // Store the chart object on el so we can get it next time
    $el.data("state", {
      selection: selection
    });
  }
  d3.select(el).select("svg").remove();
  showRadar(data);
  
  // Now, the code that'll run every time a value is rendered...
  
  // Retrieve the chart and selection we created earlier
  var state = $el.data("state");
  
};

// Tell Shiny about our new output binding
Shiny.outputBindings.register(binding, ".hallmark-chart");

})();
