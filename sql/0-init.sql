CREATE DATABASE "agenda";

\c agenda;

CREATE TABLE "users" (
    "id" SERIAL PRIMARY KEY,
    "name" VARCHAR(255) UNIQUE NOT NULL,
    "password" VARCHAR(255) NOT NULL
);

INSERT INTO "users" ("name", "password")
    VALUES ('tnegri', 'a190c5df-bd09-4517-bb32-dc3230f1b604');
