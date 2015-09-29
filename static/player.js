function ViewModel() {
    var self = this;
    self.formula = ko.observable("");
    self.result = ko.observable(0);
    self.error = ko.observable("");

    self.numbers = ko.observableArray([]);
    self.target = ko.observable(0);

    self.inputVisible = ko.computed(function() {
	return self.numbers().length > 0;
    });

    self.eval = function () {
	var f = self.formula();
	self.error("");
	self.result("");
	$.get("/api/eval/" + encodeURIComponent(f), null, function(v) {
	    self.result(v);
	}).fail(function() {
	    self.error("invalide Formel");
	});
    };

    self.queryRound = function () {
	$.get("/api/round", null, function(res) {
	    if (res) {
		self.params.numbers(res.numbers);
		self.params.target(res.target);
	    } else {
		setTimeout (self.queryRound, 500);
	    }
	}).fail(function() {
	    self.numbers.removeAll();
	    self.target(0);
	    setTimeout (self.queryRound, 500);
	});
    };
    
    self.queryRound();
};

$(function() {
    ko.applyBindings(new ViewModel());
});
