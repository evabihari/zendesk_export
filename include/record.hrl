-define(ZENDESK_URL,"XXXXXXXXXXXXX").
-define(USER,"YYYYYYYYYYYYY").
-define(PWD,"ZZZZZZZZZZZ").
-define(DIR,"/tmp/").

-record(groups,{
	  url,
	  id,
	  name,
	  deleted,
	  created_at,
	  updated_at
	 }).

-record(tickets,{
	  url,
	  id,
	  external_id,
	  via,
	  created_at,
	  updated_at,
	  type,
	  subject,
	  raw_subject,
	  description,
	  priority,
	  status,
	  recipient,
	  requester_id,
	  submitter_id,
	  assignee_id,
	  organization_id,
	  group_id,
	  collaborator_ids,
	  forum_topic_id,
	  problem_id,
	  has_incidents,
	  is_public,
	  due_at,
	  tags,
	  custom_fields,
	  satisfaction_rating,
	  sharing_agreement_ids,
	  fields,
	  brand_id,
	  allow_channelback,
	  generated_timestamp}).

-record(field,{
	  id,
	  value}).

	  
-record(comment, {
	  id,
	  type,
	  body,
	  html_body,
	  public,
	  created_at,
	  author_id,
	  attachments,
	  metadata,
	  via}).
	  
-record(attachment, {
	  id,
	  file_name,
	  content_url,
	  content_type,
	  size,
	  thumbnails}).

-record(organization, {
	id,
	url,
	external_id,
	name,
	created_at,
	updated_at,
	domain_names,
	details,
	notes,
	group_id}).
