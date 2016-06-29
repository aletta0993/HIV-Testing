var chart = d3.parsets().dimensions(["HIV Test Today","Religious Level","HIV Tested Last Year?","Ever HIV Tested?",
	"HIV Info on Billboards?","Time in the US","Unprotected Sex","Education", "Live with Partner"]);

var vis = d3.select("#vis").append("svg")
    .attr("width", chart.width())
    .attr("height", chart.height());

// var vis_relig = d3.select(".sub").append("svg")
//    .attr("width", chart.width())
//    .attr("height", chart.height());

d3.csv("viz.csv", function(error, csv) {
	vis.datum(csv).call(chart);
//	vis_relig.datum(csv).call(d3.parsets().dimensions(["HIV Test Today", "Religious Level"]));
});






