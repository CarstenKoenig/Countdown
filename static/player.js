function ViewModel() {
    var self = this;
    self.gotBusted = ko.observable(false);

    self.goal = ko.observable(null);
    self.numbers = ko.observableArray();
    self.isRunning = ko.observable(false);
    self.secondsLeft = ko.observable(null);
    self.scores = ko.observableArray();
    self.isWaiting = ko.computed (function() {
	return !self.isRunning();
    });

    self.formula = ko.observable("");
    self.result = ko.observable(0);
    self.error = ko.observable("");

    self.resetValues = function() {
	self.goal(null);
	self.numbers.removeAll();
	self.isRunning(false);
	self.secondsLeft(null);
	self.scores.removeAll();
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
		self.scores(res.scoreBoard.map(function(t) {
		    return { name: t[0], diff: t[1] };
		}));
	    } else {
		self.scores.removeAll();
	    }
	} else {
	    self.resetValue();
	}
    };

    self.setError = function (err) {
	if (err.responseText) {
	    self.error(err.responseText);
	} else {
	    self.error("Verbindung verloren!");
	}
    };

    self.queryState = function () {
	if (!self.gotBusted()) {
	    timer.pause();
	    $.ajax({
		url: "/api/current", 
		cache: false,
		success: function(res) {
		    self.setValues(res);
		    timer.play();
		}
	    }).fail(function(err) {
		self.resetValues();
		self.setError(err);
		self.bust();
	    });
	}
    };

    self.eval = function () {
	if (!self.gotBusted()) {
	    var f = self.formula();
	    self.error("");
	    self.result("");
	    $.get("/api/eval/" + encodeURIComponent(f), null, function(res) {
		if (res.info != "OK") {
		    self.error(res.info);
		} else {
		    self.result(res.value);
		}
	    }).fail(self.setError);
	}
    };

    self.bust = function () {
	self.gotBusted(true);
	timer.stop();
	resetValues();
    };

    var timer = $.timer(self.queryState, 500, true);
};

$(function() {
    ko.applyBindings(new ViewModel());
});
