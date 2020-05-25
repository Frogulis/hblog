DROP TABLE Post;
DROP Table User;

CREATE TABLE User (id INTEGER PRIMARY KEY, Username TEXT, PasswordHash TEXT, HashSalt NUMBER, UserRole TEXT);
insert into User values (1, 'jamie', '8302914017327024210', 7, 'Admin');

CREATE TABLE Post (id INTEGER PRIMARY KEY, PostId TEXT UNIQUE, Title TEXT, Author TEXT, HtmlContent BLOB, UploadDate TEXT, PublishDate TEXT);
INSERT INTO Post VALUES (1, '2020-05-07-01', 'A sample post', 'Jamie Hoffmann', '<article><p>What is going on here?!</p><p>It is a <b>post</b>!</p></article>','2020-05-07T10:00:00Z','2020-05-07T10:00:00Z');
INSERT INTO Post VALUES (2, '2020-05-09-01', 'Saturday post 1', 'Jamie Hoffmann', '<article><p>I love Saturday!</p></article>','2020-05-09T10:00:00Z',NULL);
INSERT INTO Post VALUES (3, '2020-05-09-02', 'Saturday post 2', 'Jamie Hoffmann', '<article><p>Still love Saturday!</p></article>','2020-05-09T12:00:00Z','2020-05-09T12:00:00Z');
