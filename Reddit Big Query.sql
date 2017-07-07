SELECT
  created_utc,
  score,
  body
FROM
  (
  SELECT
    rand() as random,
    created_utc,
    score,
    body
    
  FROM
    `fh-bigquery.reddit_comments.2016*`
  WHERE
    subreddit = 'NoMansSkyTheGame'
  ORDER BY
    random
   )
LIMIT 15000