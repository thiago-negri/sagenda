\c sagenda;

CREATE TABLE "users" (
    "id" SERIAL PRIMARY KEY,
    "name" VARCHAR(255) UNIQUE NOT NULL,
    "password" VARCHAR(255) NOT NULL
);

-- tnegri:a190c5df-bd09-4517-bb32-dc3230f1b604
INSERT INTO "users" ("name", "password")
    VALUES ('tnegri',
            '$2a$12$KIEoGmu3sMrtsgKZN/lKb.Js9CBnqn58xi2GGuWhpKoJrXM1rAuMO');
