CREATE INDEX messages_fts ON batyr.messages
  USING gin(to_tsvector('english',
	  coalesce(subject, '') || coalesce(thread, '') || coalesce(body, '')));
