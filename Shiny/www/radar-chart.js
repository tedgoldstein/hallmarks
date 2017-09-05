hallmarkOrder = [
    "Evading Growth Suppressors",
    "Evading Immune Destruction",
    "Replicative Immortality",
    "Tumor Promoting Inflammation",
    "Tissue Invasion and Metastasis",
    "Sustained Angiogenesis",
    "Genome Instability",
    "Resisting Cell Death",
    "Reprogramming Energy Metabolism",
    "Sustaining Proliferative Signaling"
];

hallmarkOrderMap = {};

for (var i = 0; i < hallmarkOrder.length; i++) {
    var o = hallmarkOrder[i];
    hallmarkOrderMap[o.toLowerCase()] = i;
}


function wrap(text) {
  text.each(function() {
    var text = d3.select(this),
        x = text.attr("x"),
        words = text.text().split(/\s+/).reverse(),
        word,
        line = [],
        lineNumber = text.classed("top") ? - words.length : 0,
        lineHeight = 1.1, // ems
        y = text.attr("y"),
        dy = parseFloat(text.attr("dy")),
        tspan = text.text(null).append("tspan").attr("x", 0).attr("y", y).attr("dy", dy + "em");
    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      line.pop();
      tspan.text(line.join(" "));
      line = [word];
      tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
    }
  });
}
var RadarChart = {
  defaultConfig: {
    containerClass: 'radar-chart',
    w: 800,
    h: 600,
    factor: 0.85,
    factorLegend: 1,
    levels: 3,
    levelTick: false,
    TickLength: 10,
    maxValue: 0,
    minValue: 0,
    radians: 2 * Math.PI,
    color: d3.scale.category10(),
    axisLine: true,
    axisText: true,
    zodiac: false,
    circles: true,
    radius: 3,
    open: false,
    backgroundTooltipColor: "#555",
    backgroundTooltipOpacity: "0.7",
    tooltipColor: "white",
    axisJoin: function(d, i) {
      return d.className || i;
    },
    tooltipFormatValue: function(d) {
      return d;
    },
    tooltipFormatClass: function(d) {
      return d;
    },
    transitionDuration: 300
  },
  chart: function() {
    // default config
    var cfg = Object.create(RadarChart.defaultConfig);
    function setTooltip(tooltip, msg){
      if(msg === false || msg == undefined){
        tooltip.classed("visible", 0);
        tooltip.select("rect").classed("visible", 0);
      }else{
        tooltip.classed("visible", 1);

        var container = tooltip.node().parentNode;
        var coords = d3.mouse(container);

        tooltip.select("text").classed('visible', 1).style("fill", cfg.tooltipColor);
        var padding=5;
        var bbox = tooltip.select("text").text(msg).node().getBBox();

        tooltip.select("rect")
        .classed('visible', 1).attr("x", 0)
        .attr("x", bbox.x - padding)
        .attr("y", bbox.y - padding)
        .attr("width", bbox.width + (padding*2))
        .attr("height", bbox.height + (padding*2))
        .attr("rx","5").attr("ry","5")
        .style("fill", cfg.backgroundTooltipColor).style("opacity", cfg.backgroundTooltipOpacity);
        tooltip.attr("transform", "translate(" + (coords[0]+10) + "," + (coords[1]-10) + ")")
      }
    }
    function radar(selection) {
      selection.each(function(data) {
        var container = d3.select(this);
        var tooltip = container.selectAll('g.tooltip').data([data[0]]);

        var tt = tooltip.enter()
        .append('g')
        .classed('tooltip', true)

        tt.append('rect').classed("tooltip", true);
        tt.append('text').classed("tooltip", true);

        debugger
        if (data.length == 0)
            return
        // allow simple notation
        data = data.map(function(datum) {
          if(datum instanceof Array) {
            datum = {axes: datum};
          }
          return datum;
        });

        var maxValue = Math.max(cfg.maxValue, d3.max(data, function(d) {
          return d3.max(d.axes, function(o){ return o.value; });
        }));
        maxValue -= cfg.minValue;


        var allAxis = data[0].axes.map(function(i, j){ return {name: i.axis, xOffset: (i.xOffset)?i.xOffset:0, yOffset: (i.yOffset)?i.yOffset:0}; });
        var total = allAxis.length;
        var radius = cfg.factor * Math.min(cfg.w / 2, cfg.h / 2);
        var radius2 = Math.min(cfg.w / 2, cfg.h / 2);

        container.classed(cfg.containerClass, 1);

        function getPosition(i, range, factor, func){
          factor = typeof factor !== 'undefined' ? factor : 1;
          return range * (1 - factor * func(i * cfg.radians / total));
        }
        function getHorizontalPosition(i, range, factor){
          return getPosition(i, range, factor, Math.sin);
        }
        function getVerticalPosition(i, range, factor){
          return getPosition(i, range, factor, Math.cos);
        }

        // levels && axises
        var levelFactors = d3.range(0, cfg.levels).map(function(level) {
          return radius * ((level + 1) / cfg.levels);
        });

        var levelGroups = container.selectAll('g.level-group').data(levelFactors);

        levelGroups.enter().append('g');
        levelGroups.exit().remove();

        levelGroups.attr('class', function(d, i) {
          return 'level-group level-group-' + i;
        });

        var levelLine = levelGroups.selectAll('.level').data(function(levelFactor) {
          return d3.range(0, total).map(function() { return levelFactor; });
        });

        levelLine.enter().append('line');
        levelLine.exit().remove();

        if (cfg.levelTick){
          levelLine
          .attr('class', function(levelFactor, i){
            if (radius == levelFactor) {
              return "levelFactor"
            } else {
              return "level"
            }
          })
          .attr('x1', function(levelFactor, i){
            if (radius == levelFactor) {
              return getHorizontalPosition(i, levelFactor);
            } else {
              return getHorizontalPosition(i, levelFactor) + (cfg.TickLength / 2) * Math.cos(i * cfg.radians / total);
            }
          })
          .attr('y1', function(levelFactor, i){
            if (radius == levelFactor) {
              return getVerticalPosition(i, levelFactor);
            } else {
              return getVerticalPosition(i, levelFactor) - (cfg.TickLength / 2) * Math.sin(i * cfg.radians / total);
            }
          })
          .attr('x2', function(levelFactor, i){
            if (radius == levelFactor) {
              return getHorizontalPosition(i+1, levelFactor);
            } else {
              return getHorizontalPosition(i, levelFactor) - (cfg.TickLength / 2) * Math.cos(i * cfg.radians / total);
            }
          })
          .attr('y2', function(levelFactor, i){
            if (radius == levelFactor) {
              return getVerticalPosition(i+1, levelFactor);
            } else {
              return getVerticalPosition(i, levelFactor) + (cfg.TickLength / 2) * Math.sin(i * cfg.radians / total);
            }
          })
          .attr('transform', function(levelFactor) {
            return 'translate(' + (cfg.w/2-levelFactor) + ', ' + (cfg.h/2-levelFactor) + ')';
          });
        }
        else{
          levelLine
          .attr('class', 'level')
          .attr('x1', function(levelFactor, i){ return getHorizontalPosition(i, levelFactor); })
          .attr('y1', function(levelFactor, i){ return getVerticalPosition(i, levelFactor); })
          .attr('x2', function(levelFactor, i){ return getHorizontalPosition(i+1, levelFactor); })
          .attr('y2', function(levelFactor, i){ return getVerticalPosition(i+1, levelFactor); })
          .attr('transform', function(levelFactor) {
            return 'translate(' + (cfg.w/2-levelFactor) + ', ' + (cfg.h/2-levelFactor) + ')';
          });
        }
        if(cfg.axisLine || cfg.axisText) {
          var axis = container.selectAll('.axis').data(allAxis);

          var newAxis = axis.enter().append('g');
          if(cfg.axisLine) {
            newAxis.append('line');
          }
          if(cfg.axisText) {
            newAxis.append('text');
          }

          axis.exit().remove();

          axis.attr('class', 'axis');

          if(cfg.axisLine) {
            axis.select('line')
            .attr('x1', cfg.w/2)
            .attr('y1', cfg.h/2)
            .attr('x2', function(d, i) { return (cfg.w/2-radius2)+getHorizontalPosition(i, radius2, cfg.factor); })
            .attr('y2', function(d, i) { return (cfg.h/2-radius2)+getVerticalPosition(i, radius2, cfg.factor); });
          }

          if(cfg.axisText) {
            axis.select('text')
            .attr('class', function(d, i){
              var p = getHorizontalPosition(i, 0.5);
              var q = getVerticalPosition(i, 0.5);

              return 'legend ' +
              ((p < 0.4) ? 'left' : ((p > 0.6) ? 'right' : 'middle'))
                + (q < 0.5 ? " top" : "");
            })
            .attr('dy', function(d, i) {
              var p = getVerticalPosition(i, 0.5);
              return ((p < 0.1) ? '1em' : ((p > 0.9) ? '0' : '0.5em'));
            })
            .text(function(d) { return d.name; })
            .attr('x', function(d, i){ return d.xOffset+ (cfg.w/2-radius2)+getHorizontalPosition(i, radius2, cfg.factorLegend); })
            .attr('y', function(d, i){ return d.yOffset+ (cfg.h/2-radius2)+getVerticalPosition(i, radius2, cfg.factorLegend); })
            .call(wrap)
          }
        }

        // content
        data.forEach(function(d){
          d.axes.forEach(function(axis, i) {
            axis.x = (cfg.w/2-radius2)+getHorizontalPosition(i, radius2, (parseFloat(Math.max(axis.value - cfg.minValue, 0))/maxValue)*cfg.factor);
            axis.y = (cfg.h/2-radius2)+getVerticalPosition(i, radius2, (parseFloat(Math.max(axis.value - cfg.minValue, 0))/maxValue)*cfg.factor);
          });
        });
        var polygon = container.selectAll(".area").data(data, cfg.axisJoin);

        var polygonType = 'polygon';
        if (cfg.open) {
          polygonType = 'polyline';
        }

        polygon.enter().append(polygonType)
        .classed({area: 1, 'd3-enter': 1})
        .on('mouseover', function (dd){
          d3.event.stopPropagation();
          container.classed('focus', 1);
          d3.select(this).classed('focused', 1);
          setTooltip(tooltip, cfg.tooltipFormatClass(dd.className));
        })
        .on('mouseout', function(){
          d3.event.stopPropagation();
          container.classed('focus', 0);
          d3.select(this).classed('focused', 0);
          setTooltip(tooltip, false);
        });

        polygon.exit()
        .classed('d3-exit', 1) // trigger css transition
        .transition().duration(cfg.transitionDuration)
        .remove();

        /*
           slows it down too much
        $(".all-sample-info").css("background-color", "")
        document.ROWCOLORSHACK = {}
        polygon
        .each(function(d, i) {
            color = cfg.color(i);
            $(".sample-"+d.className).css("background-color", color)
            document.ROWCOLORSHACK[d.className] = color;
        })
        */

        polygon
        .each(function(d, i) {
          var classed = {'d3-exit': 0}; // if exiting element is being reused
          classed['radar-chart-serie' + i] = 1;
          if(d.className) {
            classed[d.className] = 1;
          }
          d3.select(this).classed(classed);
        })
        // styles should only be transitioned with css
        .style('stroke', function(d, i) { return cfg.color(i); })
        .style('fill', function(d, i) { return cfg.color(i); })
        .transition().duration(cfg.transitionDuration)
        // svg attrs with js
        .attr('points',function(d) {
          return d.axes.map(function(p) {
            return [p.x, p.y].join(',');
          }).join(' ');
        })
        .each('start', function() {
          d3.select(this).classed('d3-enter', 0); // trigger css transition
        });

        if(cfg.circles && cfg.radius) {

          var circleGroups = container.selectAll('g.circle-group').data(data, cfg.axisJoin);

          circleGroups.enter().append('g').classed({'circle-group': 1, 'd3-enter': 1});
          circleGroups.exit()
          .classed('d3-exit', 1) // trigger css transition
          .transition().duration(cfg.transitionDuration).remove();

          circleGroups
          .each(function(d) {
            var classed = {'d3-exit': 0}; // if exiting element is being reused
            if(d.className) {
              classed[d.className] = 1;
            }
            d3.select(this).classed(classed);
          })
          .transition().duration(cfg.transitionDuration)
          .each('start', function() {
            d3.select(this).classed('d3-enter', 0); // trigger css transition
          });

          var circle = circleGroups.selectAll('.circle').data(function(datum, i) {
            return datum.axes.map(function(d) { return [d, i]; });
          });

          circle.enter().append('circle')
          .classed({circle: 1, 'd3-enter': 1})
          .on('mouseover', function(dd){
            d3.event.stopPropagation();
            setTooltip(tooltip, cfg.tooltipFormatValue(dd[0].value));
            //container.classed('focus', 1);
            //container.select('.area.radar-chart-serie'+dd[1]).classed('focused', 1);
          })
          .on('mouseout', function(dd){
            d3.event.stopPropagation();
            setTooltip(tooltip, false);
            container.classed('focus', 0);
            //container.select('.area.radar-chart-serie'+dd[1]).classed('focused', 0);
            //No idea why previous line breaks tooltip hovering area after hoverin point.
          });

          circle.exit()
          .classed('d3-exit', 1) // trigger css transition
          .transition().duration(cfg.transitionDuration).remove();

          circle
          .each(function(d) {
            var classed = {'d3-exit': 0}; // if exit element reused
            classed['radar-chart-serie'+d[1]] = 1;
            d3.select(this).classed(classed);
          })
          // styles should only be transitioned with css
          .style('fill', function(d) { return cfg.color(d[1]); })
          .transition().duration(cfg.transitionDuration)
          // svg attrs with js
          .attr('r', cfg.radius)
          .attr('cx', function(d) {
            return d[0].x;
          })
          .attr('cy', function(d) {
            return d[0].y;
          })
          .each('start', function() {
            d3.select(this).classed('d3-enter', 0); // trigger css transition
          });

          //Make sure layer order is correct
          var poly_node = polygon.node();
          poly_node.parentNode.appendChild(poly_node);

          var cg_node = circleGroups.node();
          cg_node.parentNode.appendChild(cg_node);

          // ensure tooltip is upmost layer
          var tooltipEl = tooltip.node();
          tooltipEl.parentNode.appendChild(tooltipEl);
        }
      });
    }

    radar.config = function(value) {
      if(!arguments.length) {
        return cfg;
      }
      if(arguments.length > 1) {
        cfg[arguments[0]] = arguments[1];
      }
      else {
        d3.entries(value || {}).forEach(function(option) {
          cfg[option.key] = option.value;
        });
      }
      return radar;
    };

    return radar;
  },
  draw: function(id, d, options, nrow) {
    var chart = RadarChart.chart().config(options);
    var cfg = chart.config();

    if (cfg.zodiac) {
        d3.select(id).attr("class", "radarchart-zodiac");
        cfg.axisText = false;
    } else {
        d3.select(id).attr("class", "radarchart-nozodiac");
        cfg.axisText = true;
    }

    d3.select(id).select('.hallmark-chart').remove();

    svg = d3.select(id)
    .append("div")
    .attr("class", "hallmark-chart")
    .append("svg")
    .attr("class", "hallmark-svg")
    .attr("width", cfg.w)
    .attr("height", cfg.h)
    if (cfg.zodiac) {
        svg = svg.attr("transform", "translate(160,160)") // 160 is the office into the hallmarks graphic svg
            .append("g")
            .attr("transform", "rotate(-18,240,240)") // 18 is the degrees, 240 is the center of the radar chart
    } else
        svg.attr("transform", "translate(165, 50)")


        svg
        .datum(d)
        .call(chart)

  },


  legend: function(cfg, legend) {
     $(".legend-ul").empty()

    legend = Object.values(legend)
    for (var i = 0; i < legend.length; i++) {
         var label = legend[i];
         label = label.replace(/ none/g, "")
	 var color = cfg.color(i);
         var txt = '<li> <div class="legend-input-color"> <textarea class="legend-label" rows="2" cols="60">' +  label + '</textarea> <div class="legend-color-box" style="background-color: '+ color + ';"></div> </div> </li>';
         $(".legend-ul").append(txt);
     }
  }

};

testdata = [
          { className: "Sample1", axes: [
                  { "axis" :[ "Evading_growth_suppressors"], value:650 },
                  { "axis" :[ "Evading_immune_destruction"], value:780 },
                  { "axis" :[ "Genome_instability"],value: 670},
                  { "axis" :[ "Replicative_immortality"], value: 722},
                    { "axis" :[ "Reprogramming_energy_metabolism"], value: 600},
                    { "axis" :[ "Resisting_cell_death"], value: 590},
                    { "axis" :[ "Sustained_angiogenesis"], value: 880},
                    { "axis" :[ "Sustaining_proliferative_signaling"], value:810 },
                    { "axis" :[ "Tissue_invasion_and_metastasis"], value: 822},
                    { "axis" :[ "Tumor-promoting_inflammation"], value: 700 },
           ] },
      ];

function showRadar(R) {

    R = JSON.parse(R);
    var nrow = R.nrow

    var colnames = R.colnames
    var rownames = R.rownames
    
    if (nrow == 1) rownames = [rownames];  // aRrrgh 

    var zodiac = R.zodiac
    var legend = R.legend
    var df = R.df


    var data = rownames.map( function(className, i) {
	     var axes = colnames.map(function(colname, j) {
                value = df[colname][i]
		colname = colname.replace(/[_\.]/g, " ")
		return { axis: [ colname], value: value }
	     });
	     className = className.replace(/[_\.]/g, " ")
             return { className: className, axes: axes };
          });
  var chart = RadarChart.chart();
  var w,h;

  RadarChart.defaultConfig.zodiac = zodiac;

  if (RadarChart.defaultConfig.zodiac) {
      w = 480,
      h = 480;
  } else {
      w = 600,
      h = 800;
  }
  // console.log(data);

  data.map(function(x) {
      x.axes.sort(function(a, b) {
          var aa = hallmarkOrderMap[a.axis[0].toLowerCase()];
          var ba = hallmarkOrderMap[b.axis[0].toLowerCase()];
          return ba - aa;
      });
  })


  RadarChart.defaultConfig.radius = 3;
  RadarChart.defaultConfig.w = w;
  RadarChart.defaultConfig.h = h;
  RadarChart.defaultConfig.levelTick =  true,

  RadarChart.draw("#radarchart", data, nrow);
  if (nrow > 0)
      RadarChart.legend(RadarChart.defaultConfig, legend)

}

function transpose(a) {
return Object.keys(a[0]).map(function(c) {
        return a.map(function(r) { return r[c]; });
});
}

function isNumeric(n) {
return !isNaN(parseFloat(n)) && isFinite(n);
}

function RankNormalize(a) {
var m = a.length; // m rows
var n = a[0].length; // n columns
var b = [];
b.push(a[0]);

for (var j = 1; j < m; j++)  { // foreach row
   var x = Array(n, 0.0);
   x.unshift(a[j][0]);
   b.push( x );
}

for (var i = 1; i < n; i++) { // foreach column
    var order = [];
    for (var j = 1; j < m; j++) { // foreach row
        var s = a[j][i];
        var nn = parseFloat(s);
        if (isNaN(nn)){
           nn = 0.0;
       }
       order.push({n:nn, j:j});
    }
    order.sort( function (a, b) { return a.n - b.n; });
    
    var r = 6.0 / order.length;
    var s = -3.0;
    for (var o = 0; o < order.length; o++) { // foreach row
        var oo = order[o];
        b[oo.j][i] = s;
        // console.log(o, oo.j, oo.n, a[oo.j][i], b[oo.j][i], s) 
        s += r;
    }
}
return b;
}

function FormatForRadar(rect) {
var m = rect.length; // m rows
var n = rect[0].length; // n columns
var radar = [];

for (var i = 1; i < m; i++) { // foreach row
    var obj = {
          className: rect[i][0],
          axes: []
        };
    radar.push(obj);
    for (var j = 1; j < n; j++) { // foreach column
        obj.axes.push( { axis: [rect[0][j]], value:rect[i][j] } );
    }
}
return radar;
}

