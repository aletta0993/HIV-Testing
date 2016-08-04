var chart = d3.parsets()
				.dimensions(["HIV Test Today","Religious Level","HIV Tested Last Year?","Ever HIV Tested?",
							"HIV Info on Billboards?","Time in the US","Unprotected Sex","Education", "Live with Partner"])
				.width(650);

var vis = d3.select("#content").append("svg")
    .attr("width", chart.width())
    .attr("height", chart.height());

d3.csv("viz.csv", function(error, csv) {
	vis.datum(csv).call(chart);
});






