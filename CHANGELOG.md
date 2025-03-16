# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- REST API endpoints for ICD-10 and ICD-11 catalogs
- Search functionality with diacritics folding and multi-term AND matching
- Crosswalk mapping between ICD-10 and ICD-11 codes
- Import pipeline with dry-run mode and error accumulation
- In-memory caching with cache flush admin endpoint
- RFC 7807 Problem JSON error responses
- Health check (`/healthz`) with database connectivity
- Readiness check (`/readyz`) with database ping
- Prometheus metrics endpoint (`/metrics`)
- Security headers middleware (CSP, X-Frame-Options, etc.)
- CORS middleware with configurable origins
- Request ID middleware for tracing
- Structured JSON logging
- Rate limiter (in-memory, IORef-based)
- Configuration via environment variables with `.env.example`
- Feature flags for cache and metrics
- Database connection pool sizing via `POOL_SIZE`
- Statement timeout (5 seconds) on all queries
- Swagger/OpenAPI documentation at `/swagger.json`
- API versioning under `/api/v1/`
- Docker Compose for local development with PostgreSQL

### Changed
- Migrated seed data to external fetch script (`scripts/fetch-seed.sh`)
- ADR-04: Seed data externalization to orphan branch

### Security
- Security headers on all responses
- CORS policy configurable
- Rate limiting infrastructure

## [0.1.0] - 2025-03-15

### Added
- Initial project scaffolding with Haskell, Servant, Persistent, PostgreSQL
- Data models for ICD-10, ICD-11 catalog items and crosswalk mappings
- Database migrations and seed data loading
- CSV import pipeline with validation
- Basic search and list endpoints
- Health check endpoint
- In-memory caching layer
- Multi-stage Dockerfile
- CI pipeline with hlint and weeder