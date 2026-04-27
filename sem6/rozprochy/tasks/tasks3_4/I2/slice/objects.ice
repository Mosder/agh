["java:package:gen"]
module Demo {
	interface Item {
		idempotent string getState();
		idempotent void setState(string newState);
	};
};
