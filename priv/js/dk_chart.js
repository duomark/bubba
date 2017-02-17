// Based on http://bost.ocks.org/mike/chart/time-series-chart.js
// and the description at http://bost.ocks.org/mike/chart

function bubba_time_series_chart() {
  var margin = {top: 20, right: 20, bottom: 40, left: 20},
      width = 600,
      height = 180;
      xValue = function(d) {return d[0]},
      yValue = function(d) {return d[1]},
      xScale = d3.scale.linear(),
      yScale = d3.scale.linear(),
      xAxis = d3.svg.axis().scale(xScale).orient("bottom").tickSize(6,0),
      area = d3.svg.area().x(X).y1(Y),
      line = d3.svg.line().x(X).y(Y);

  function chart(selection) {
    selection.each(function(data) {

      data = data.map(function(d,i) {
        return [xValue.call(data,d,i), yValue.call(data,d,i)];
      });

      xScale
          .domain(d3.extent(data, function(d) {return d[0]}))
          .range([0, width - margin.left - margin.right]);
      yScale
          .domain([0, d3.max(data, function(d) {return d[1]})])
          .range([height - margin.top - margin.bottom, 0]);

      var svg = d3.select(this).selectAll("svg").data([data]);

      var gEnter = svg.enter().append("svg").append("g");
      gEnter.append("path").attr("class", "area");
      gEnter.append("path").attr("class", "line");
      gEnter.append("g").attr("class", "x axis");

      svg .attr("width", width)
          .attr("height", height);

      var g = svg.select("g")
          .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

      g.select(".area")
          .attr("d", area.y0(yScale.range()[0]));

      g.select(".line")
          .attr("d", line);

      g.select(".x.axis")
          .attr("transform", "translate(0," + yScale.range()[0] + ")")
          .call(xAxis);
      });
  }

  function X(d) {return xScale(d[0])}
  function Y(d) {return yScale(d[1])}

  chart.margin = function(_) {
    if (!arguments.length) return margin;
    margin = _; return chart;
  }

  chart.width = function(_) {
    if (!arguments.length) return width;
    width = _; return chart;
  };

  chart.height = function(_) {
    if (!arguments.length) return height;
    height = _; return chart;
  };

  chart.x = function(_) {
    if (!arguments.length) return xValue;
    xValue = _; return chart;
  };

  chart.y = function(_) {
    if (!arguments.length) return yValue;
    yValue = _; return chart;
  };

  return chart;
}

