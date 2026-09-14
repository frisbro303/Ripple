use rusqlite::{params, Connection};
use tauri::Manager;

fn get_connection(app: &tauri::AppHandle) -> Result<Connection, String> {
    let dir = app.path().app_data_dir().map_err(|e| e.to_string())?;
    std::fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    let conn = Connection::open(dir.join("app.db")).map_err(|e| e.to_string())?;
    conn.execute(
        "CREATE TABLE IF NOT EXISTS ops_log (
            id TEXT PRIMARY KEY,
            created_at TEXT NOT NULL,
            entry TEXT NOT NULL,
            synced INTEGER NOT NULL DEFAULT 0
        )",
        [],
    )
    .map_err(|e| e.to_string())?;
    let _ = conn.execute(
        "ALTER TABLE ops_log ADD COLUMN synced INTEGER NOT NULL DEFAULT 0",
        [],
    );
    Ok(conn)
}

fn insert_ops(
    conn: &mut Connection,
    ops: &[serde_json::Value],
    synced: bool,
) -> Result<(), String> {
    let tx = conn.transaction().map_err(|e| e.to_string())?;
    {
        let mut stmt = tx
            .prepare("INSERT OR IGNORE INTO ops_log (id, created_at, entry, synced) VALUES (?1, ?2, ?3, ?4)")
            .map_err(|e| e.to_string())?;
        for op in ops {
            let id = op.get("id").and_then(|v| v.as_str()).ok_or("missing id")?;
            let created_at = op
                .get("created_at")
                .and_then(|v| v.as_str())
                .ok_or("missing created_at")?;
            let entry = op.get("entry").ok_or("missing entry")?;
            let entry_str = serde_json::to_string(entry).map_err(|e| e.to_string())?;
            stmt.execute(params![id, created_at, entry_str, synced as i64])
                .map_err(|e| e.to_string())?;
        }
    }
    tx.commit().map_err(|e| e.to_string())
}

#[tauri::command]
pub fn db_insert_ops(
    app: tauri::AppHandle,
    ops: Vec<serde_json::Value>,
    synced: bool,
) -> Result<(), String> {
    let mut conn = get_connection(&app)?;
    insert_ops(&mut conn, &ops, synced)
}

#[tauri::command]
pub fn db_mark_synced(app: tauri::AppHandle, ids: Vec<String>) -> Result<(), String> {
    let mut conn = get_connection(&app)?;
    let tx = conn.transaction().map_err(|e| e.to_string())?;
    {
        let mut stmt = tx
            .prepare("UPDATE ops_log SET synced = 1 WHERE id = ?1")
            .map_err(|e| e.to_string())?;
        for id in &ids {
            stmt.execute(params![id]).map_err(|e| e.to_string())?;
        }
    }
    tx.commit().map_err(|e| e.to_string())
}

#[tauri::command]
pub fn db_clear_ops(app: tauri::AppHandle) -> Result<(), String> {
    let conn = get_connection(&app)?;
    conn.execute("DELETE FROM ops_log", [])
        .map_err(|e| e.to_string())?;
    Ok(())
}

fn query_ops(conn: &Connection, where_clause: &str) -> Result<Vec<serde_json::Value>, String> {
    let sql = format!(
        "SELECT id, created_at, entry FROM ops_log {} ORDER BY created_at",
        where_clause
    );
    let mut stmt = conn.prepare(&sql).map_err(|e| e.to_string())?;

    let rows = stmt
        .query_map([], |row| {
            let id: String = row.get(0)?;
            let created_at: String = row.get(1)?;
            let entry_str: String = row.get(2)?;
            Ok((id, created_at, entry_str))
        })
        .map_err(|e| e.to_string())?;

    let mut ops = Vec::new();
    for row in rows {
        let (id, created_at, entry_str) = row.map_err(|e| e.to_string())?;
        let entry: serde_json::Value =
            serde_json::from_str(&entry_str).map_err(|e| e.to_string())?;
        ops.push(serde_json::json!({
            "id": id,
            "created_at": created_at,
            "entry": entry
        }));
    }
    Ok(ops)
}

#[tauri::command]
pub fn db_get_ops(app: tauri::AppHandle) -> Result<Vec<serde_json::Value>, String> {
    let conn = get_connection(&app)?;
    query_ops(&conn, "")
}

#[tauri::command]
pub fn db_get_pending_ops(app: tauri::AppHandle) -> Result<Vec<serde_json::Value>, String> {
    let conn = get_connection(&app)?;
    query_ops(&conn, "WHERE synced = 0")
}
