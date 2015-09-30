function ViewModel() {
    var self = this;
    self.goal = ko.observable(null);
    self.numbers = ko.observableArray();
    self.isStartable = ko.observable(false);
    self.isRunning = ko.observable(false);
    self.secondsLeft = ko.observable(null);
    self.scores = ko.observableArray();

    self.resetValues = function() {
	self.goal(null);
	self.numbers.removeAll();
	self.isStartable(false);
	self.isRunning(false);
	self.secondsLeft(null);
	self.scores.removeAll();
    };

    self.setValues = function(res) {
	if (res) {
	    self.goal(res.goal);
	    self.numbers(res.availableNrs);
	    self.isStartable(res.isStartable);
	    self.isRunning(res.isRunning);
	    self.secondsLeft(res.secondsLeft);
	    self.scores(res.scoreBoard.map(function(t) {
		return { name: t[0], diff: t[1], formula: t[2] };
	    }));
	} else {
	    self.resetValue();
	}
    };

    self.queryState = function () {
	$.ajax({
	    url: "/api/current", 
	    cache: false,
	    success: function(res) {
		self.setValues(res);
	    }
	}).fail(function() {
	    self.resetValues();
	});
    };
    
    self.startRound = function () {
	if (!self.isStartable()) {
	    return; 
	} else {
	    $.post("/api/start", null, function(res) {
		self.setValues(res);
	    }).fail(function() {
		self.resetValues();
	    })
	}
    };

    var timer = $.timer(self.queryState, 500, true);
};

$(function() {
    ko.applyBindings(new ViewModel());
});
