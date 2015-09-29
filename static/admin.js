function ViewModel() {
    var self = this;
    self.numbers = ko.observableArray();
    self.target = ko.observable(0);

    self.scores = ko.observableArray();

    self.canStart = ko.observable(false);

    self.queryCanStart = function () {
	$.get("/api/canStart", null, function(res) {
	    self.canStart(res);
	    setTimeout (self.queryCanStart, 500);
	    }
	}).fail(function() {
	    self.canStart(false);
	    setTimeout (self.queryCanStart, 500);
	});
    };

    self.queryRound = function () {
	$.get("/api/round", null, function(res) {
	    if (res) {
		self.numbers(res.params.numbers);
		self.target(res.params.target);
	    } else {
		setTimeout (self.queryRound, 500);
	    }
	}).fail(function() {
	    self.numbers.removeAll();
	    self.target(0);
	    setTimeout (self.queryRound, 500);
	});
    };
    
    self.startRound = function () {
	if (!self.canStart()) { 
	    return; 
	} else {
	    $.post("/api/start", null, function(res) {
		if (res) {
		    self.numbers(res.params.numbers);
		    self.target(res.params.target);
		}
	    }).fail(function() {
		self.numbers.removeAll();
		self.target(0);
	    })
	}
    };

    self.getScores = function () {
	$.get("/api/scores", null, function(res) {
	    if (res) {
		self.scores(res.map(function(t) { 
		    return {name: t[0], diff: t[1]}; 
		}));
	    };
	    setTimeout (self.getScores, 1000);
	}).fail(function() {
	    setTimeout (self.getScores, 1000);
	});
    };

    self.queryRound();
    self.queryCanStart();
    self.getScores();
};

$(function() {
    ko.applyBindings(new ViewModel());
});
