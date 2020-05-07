DROP TABLE Post;
CREATE TABLE Post (id INTEGER PRIMARY KEY, PostId TEXT, Title TEXT, HtmlContent BLOB, UploadDate TEXT, PublishDate TEXT);
INSERT INTO Post VALUES (1, '2020-05-07-01', 'A sample post', '<article><p>What is going on here?!</p><p>It is a <b>post</b>!</p></article>','2020-05-07T10:00:00Z',NULL);
