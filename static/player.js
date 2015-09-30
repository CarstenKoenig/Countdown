function ViewModel() {
    var self = this;
    self.goal = ko.observable(null);
    self.numbers = ko.observableArray();
    self.isRunning = ko.observable(false);
    self.secondsLeft = ko.observable(null);

    self.formula = ko.observable("");
    self.result = ko.observable(0);
    self.error = ko.observable("");

    self.resetValues = function() {
	self.goal(null);
	self.numbers.removeAll();
	self.isRunning(false);
	self.secondsLeft(null);
    };

    self.setValues = function(res) {
	if (res) {
	    self.goal(res.goal);
	    self.numbers(res.availableNrs);
	    self.isRunning(res.isRunning);
	    self.secondsLeft(res.secondsLeft);

	    if (!self.isRunning()) {
		self.formula("");
		self.error("");
	    }
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


    self.eval = function () {
	var f = self.formula();
	self.error("");
	self.result("");
	$.get("/api/eval/" + encodeURIComponent(f), null, function(guess) {
	    if (guess.guessInfo != "OK") {
		self.error(guess.guessInfo);
	    } else {
		self.result(guess.guessValue);
	    }
	}).fail(function() {
	    self.error("invalide Formel");
	});
    };

    var timer = $.timer(self.queryState, 500, true);
};

$(function() {
    ko.applyBindings(new ViewModel());
});
