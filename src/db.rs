//! Database related functionality goes here uwu
use sqlx::SqlitePool;
use once_cell::sync::Lazy;

// Frickin' ambient authority. lol
/// This will be set exactly once, right there in the `main` functions.
/// It therefore does not require synchronization.
/// `SqlitePool` does its own synchronization, and is used via shared references.
pub(crate) static POOL: Lazy<SqlitePool> = Lazy::new(|| SqlitePool::connect_lazy("sqlite://dev.db").unwrap());

pub(crate) async fn initialize() -> Result<(), ::sqlx::Error> {
    POOL.acquire().await.map(|_| ())
}

pub(crate) async fn shutdown() {
    POOL.close().await
}
