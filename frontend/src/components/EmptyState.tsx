export function EmptyState({ hasFilters }: { hasFilters: boolean }) {
  return (
    <div className="empty-state">
      <h3>{hasFilters ? "No jobs match your filters" : "No matching roles in the last 24 hours"}</h3>
      <p>
        {hasFilters
          ? "Try clearing the search or company filter."
          : "That's expected on quiet days — the tracked companies simply may not have posted a new London role since the last crawl. Check back after the next scheduled run."}
      </p>
    </div>
  );
}
