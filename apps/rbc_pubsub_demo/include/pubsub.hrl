-record(pubsub_message, {sender, data, date}).
-type pubsub_message() :: #pubsub_message{}.

-type already_subscribed() :: {error, pid_is_already_subscribed, [pid()]}.
-type is_not_subscribed() :: {error, pid_is_not_subscrubed, [pid()]}.
